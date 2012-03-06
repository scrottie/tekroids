#!/usr/bin/perl

=for comment

Multiplayer telnet-based Asteroids clone for Tektronix 4014 terminal emulation.

This runs as a daemon that people can telnet in to.  It requires an xterm with Tektronix emulation.

Connected players can find each other on the map and co-operate.

Control the ship with the mouse.  It auto-fires constantly (as there's no good way I've found to read
button presses while keeping things real-time).  The further the cursor is from your ship, the faster
you accelerate in that direction.

When you die, your ship vanishes and all of the asteroids except the one that killed you vanish.  If you
sit there long enough, eventually it should come around again.  To re-join the game, disconnect 
(probably control-"]" then "c", then enter) and then re-connect.  Until the server gets restarted, the
same set of asteroids will be floating around.  More aren't added once the game starts.  It's possible
to clear the universe of asteroids.

Some info on the Tekronics 4014 terminal and its emulation:  
* http://invisible-island.net/xterm/xterm.faq.html
* http://www.chilton-computing.org.uk/acd/icf/terminals/p005.htm 
* http://slowass.net/~scott/tmp/tek-4014-um.pdf

TODO

o. able to view objects on the other side of the wrap around -- special cases to the draw loop when on an any edge
o. between waves, let people tweak game variables
o. irc bot that will notify opt-ed in users when someone joins the game
o. <hobbs> next unless ($x_a - $x_b) ** 2 + ($y_a - $y_b) ** 2 <= $size ** 2
o. debris fields when the ship exploids or the smallest asteroid chunks finally get destroyed
o. wrap around the edges of the world isn't graceful; you can't see past the edges, so asteroids etc suddenly appear
o. maybe peoples ships should have varied shapes
o. 760 or 768?  get it right
o. asteroid gravitational attraction and merging?  tried merging; very interesting indeed
o. parallex stars?  starfield generator?  some kind of point of reference would make stuff less confusing
o. http://www.youtube.com/watch?v=psaM7kK5Toc ... tune this thing up better so it looks like that
o. score.  should get points for survival time and for hits.  would mean associating each missile with the player that show it.
o. game challenges.  limited fuel; half way intelligent enemies; left to right side scroller with levelish stuff (objects pre-placed according to a level design); levels could include ships that blow up into essentially missiles to take other stuff with them; large bodies with gravity; space squid

PERFORMANCE

I really expected that this would handle thousands of asteroids gracefully, but it bogs with just a few hundred.
Not the client -- the server.  This.

Last time this was timed:

collision => 430364019768
draw      => 196395798904
animate   => 430356592784

I figured collision detection would be taking the vast majority of the CPU, but it doesn't take any more than
just advacing objects on their trajectories, which is fair micro-optimized.

NYTProf coredumps, probably because of Coro.

Objects are kept sorted by X.  quicksort runs very near O(n) time when the data is almost entirely already sorted,
so keeping it sorted as items are added is pretty inexpensive.

Most of the inlining things and breaking encapsulation and other muckiness seems to be for naught and should 
probably be ripped back out.

=cut

use strict;
use warnings;

use sort '_quicksort';

use Coro;
use Coro::Socket;
use Coro::Event;
use Coro::Timer 'sleep';

use IO::Handle;
use IO::Socket;

# use POSIX qw(:errno_h);
# use Time::HiRes 'gettimeofday';
use Math::Trig 'deg2rad';
use Fcntl;
use Carp;

$SIG{__DIE__} = sub { Carp::cluck @_; die @_ };
# $SIG{__WARN__} = sub { Carp::cluck @_ };

sub opt ($) { scalar grep $_ eq $_[0], @ARGV }
sub arg ($) { my $opt = shift; my $i=1; while($i<=$#ARGV) { return $ARGV[$i] if $ARGV[$i-1] eq $opt; $i++; } }

# my $world_x = 8192;
# my $world_y = 8192;
my $world_x = 2000;
my $world_y = 2000;

my @objects = map { asteroid->new } 1..15;
# push @objects, map { star->new } 1..20; # XXX give points of reference for testing

my $num_players = 0;

my $port = 2003;

#use Benchmark;
#my %timers;

listen_again:
my $telnetlisten = eval { 
    warn "listening on port $port";
    Coro::Socket->new(
        LocalPort => $port,
        # Type      => SOCK_STREAM,
        Listen    => SOMAXCONN,
    ); 
} or do { $port++; goto listen_again; };

async {

    # animate calls, collision detection

    my $timestamp = Time::HiRes::gettimeofday; 

    while(1) {

        #
        # collision detection
        #

#        if( ! grep ref $_ eq 'ship', @objects ) { # XXXX

            # no one looking?  nothing happens

            # for now, only interested in ship<->asteroid collisions and bullet<->asteroid collisions
    
            test_collided( );
            # $timers{collision} += timeit(1, sub { test_collided( ) } ); # XXX
    
#        };

        #
        # re-initialize board as needed
        #

        my %nums = ( asteroid => 0, missile => 0, ship => 0 );
        for my $ob (@objects) { $nums{ref $ob}++ }
        push @objects, map { asteroid->new } 1..15 if ! $nums{asteroid};

        #
        # move things
        #

        for my $ob ( @objects ) {
            $ob->animate;
        }

        #$timers{animate} += timeit(1, sub {
        #    for my $ob ( @objects ) {
        #        $ob->animate;
        #    }
        #}); # XXX

        #
        # reap things lost in collisions or animated until they're expired
        #
 
        @objects = grep { ! $_->destroyed } @objects;

        #
        #
        #
    
        my $new_ts = Time::HiRes::gettimeofday;
        my $sleep_for = 1/15 - ( $new_ts - $timestamp );
        $sleep_for = 0 if $sleep_for < 0;
        $timestamp = $new_ts;
        Coro::Timer::sleep $sleep_for;

        #
        #
        #

        #for my $k (keys %timers) {
        #    print "$k => $timers{$k}  ";
        #}
        #print "\n";

    }

};

while(1) {

    my $client = $telnetlisten->accept() or die $!;

    async {

        $client->autoflush(1);

        $client->print(
            chr(255), chr(251), chr(1),    # telnet will echo
            chr(255), chr(251), chr(3),    # telnet will supress go-ahead
            chr(27), "[?38h",   # TEK mode from vtwhatever mode
            chr(27), chr(12),   # clear screen 
            chr(13+16),                           # graphics mode
        ); 

        my $line = '';      # character input buffer
        my $message = '';   # score / messages / diagnostic output to the player
        my $frame;
        my $timestamp = Time::HiRes::gettimeofday;
 
        my $fps_count = 0;
        my $fps_count_last = 0;
        my $fps_second = time;

        my $ship = ship->new;
        push @objects, $ship;

        $num_players++;
        print "got client connection: " . scalar(localtime) . " num players: $num_players\n";

      client_connected:
        while(1) {
    
            #
            # draw
            #
    
            if( my  $destroyed = $ship->destroyed ) {

                # draw missiles and other ships, but not our own ship, and only the asteroid that killed us

                my $buf = '';
                for my $ob ( @objects ) {
                    $buf .= $ob->draw( $ship ) if $destroyed == $ob or ref $ob eq 'missile' or (ref $ob eq 'ship' and $ob ne $ship);
                }
                $client->print( redraw_screen($message), $buf );

            } else {

                my $buf = draw_stuff($ship);
                $client->print( redraw_screen($message), $buf );  # wait on printing the clear screen until we have data to send to minimize flicker

                # for my $ob ( @objects ) {
                #     $client->print( $ob->draw( $ship ) );
                # }

                #$timers{draw} += timeit(1, sub {
                #    for my $ob ( @objects ) {
                #        $client->print( $ob->draw( $ship ) );
                #    }
                #});

            }

            #
            # input
            #
    
            $client->print(
                chr(27), chr(26),              # Tek GIN mode (Graphics IN)
                chr(27), chr(5),               # escape-ENQ -- request mouse position from terminal
            );
    
            my $line = '';
            while( length($line) < 4 ) {
                $client->read(my $c, 1);
                defined $c or do { print "null read: character not defined; exiting for this client\n"; last client_connected; }
;
                length $c or do { print "zero length but not null read... meh? lasting I guess\n"; last client_connected; };
                $line .= $c;
                if(length($line) == 3 and ord(substr($line, 0, 1)) == 255) {
                    # telnet shit
                    # XXX should stop looking for these after we've been connected a couple of seconds so they don't interfere with legimate mouse reads
                    substr($line, 0, 3, '');
                    next;
                }
            }

            my $mouse_x = ((ord(substr $line, 0, 1)&0b011111)<<5) | (ord(substr $line, 1, 1)&0b011111);
            my $mouse_y = 768 - (((ord(substr $line, 2, 1)&0b011111)<<5) | (ord(substr $line, 3, 1)&0b011111));
    
            #
            # ship control
            #
    
            if( ! $ship->destroyed ) {
    
                my $x_delta = $mouse_x - 1024/2;
                my $y_delta = $mouse_y - 760/2;
    
                $ship->rot = atan2( $y_delta, $x_delta ) * 57.2;  # +/- pi to degrees
    
                my $distance = sqrt( $x_delta ** 2 + $y_delta ** 2 );
                my $thrust = 0;
                $thrust = 0.2 if $distance > 200;  # XXX tune this
                $thrust = 0.4 if $distance > 300;  # XXX tune this
                $thrust = 0.8 if $distance > 400;  # XXX tune this

                # $thrust /= 2 if $thrust > 0 and abs($ship->x_velocity) > 1.5 or abs($ship->y_velocity) > 1.5;  # XXX tune this
                # $thrust = 0 if $thrust > 0 and abs($ship->x_velocity) > 3 or abs($ship->y_velocity) > 3; # no, that'll keep them from accelerating in a different direction, which includes ever slowing down.  oops.
    
                $ship->add_thrust( $thrust );
                my $speed = sqrt( $ship->x_velocity ** 2 + $ship->y_velocity ** 2 );
                if( $speed > 3 ) {
                    $speed = 3;
                    my $angle = atan2( $ship->x_velocity, $ship->y_velocity ) * 57.2;  # +/- pi to degrees
                    $ship->x_velocity = cos(Math::Trig::deg2rad($angle)) * $speed;
                    $ship->y_velocity = sin(Math::Trig::deg2rad($angle)) * $speed;
                }
 
                my %nums = ( asteroid => 0, missile => 0, ship => 0 );
                for my $ob (@objects) { $nums{ref $ob}++ }
                if( $fps_second == time ) {
                    $fps_count++;
                } else {
                    $fps_count_last = $fps_count;
                    $fps_count = 0;    
                    $fps_second = time;
                    # if( $fps_count_last == 0 { DB::finish_profile(); exit; }
                }
                my $airspeed = sprintf "%2.1f", $speed; # sqrt( $ship->x_velocity ** 2 + $ship->y_velocity ** 2 );
                $message = 'x: ' . int($ship->x) . ' y: ' . int($ship->y) . " speed: $airspeed asteroids: $nums{asteroid} players: $nums{ship} fps: $fps_count_last";
    
            }
    
            #
            # fire new missile
            #

            $frame++;
    
            if( ! $ship->destroyed ) {
                if( ! ( $frame % 5 ) ) {   # XXX tune this
                    my $missile = missile->new;
                    $ship->copy_position_and_velocity_to( $missile );
                    $missile->animate for 1..2; # start in front of the ship
                    $missile->add_thrust( 3 );
                    push @objects, $missile;  
                }
            }
    
            #
            #
            #
    
            # usleep 16666*2;  # 16666 = 1/60th of a second # XXX use AnyEvent::Timer instead
            my $new_ts = Time::HiRes::gettimeofday;
            my $sleep_for = 1/15 - ( $new_ts - $timestamp );
            $timestamp = $new_ts;
            $sleep_for = 0 if $sleep_for < 0;
            Coro::Timer::sleep $sleep_for;
    
        }

        @objects = grep $_ ne $ship, @objects;

        $num_players--;
    
        print "client went away: $!  num_players: $num_players\n";

    }; # end client handling async block

} # end while 1

sub test_collided {

    @objects = sort { $a->x <=> $b->x } @objects;

    my $left_i = -1;
    my $right_i = 0;

    $left_i-- while $left_i > - $#objects and $objects[$left_i]->x < $world_x - 100; # start viewing both edges of the universe

    # do an n:n comparison, comparing each object to each other one, inside of a sliding window of objects
    # whose x positions are within a hard-coded range of each other

    while(1) {

        $right_i++;

      advanced_left_position:

        next if $left_i == $right_i;

        last if $right_i > $#objects;

        my $left = $objects[ $left_i ];
        my $right = $objects[ $right_i ];

        # print "test_collided: left_i: $left_i right_i: $right_i left x: @{[ $left->x ]} right x: @{[ $right->x ]}\n";

        my $x_a = $left->[2]; # $left->x; ... speed
        my $x_b = $right->[2]; # $right->x; ... speed
        $x_b -= $world_x if $x_a > $world_x - 100 and $x_b < 100;  # $x_a is on the right edge and $x_a the left
        $x_a -= $world_x if $x_b > $world_x - 100 and $x_a < 100;  # $x_a is on the left edge and $x_a the right

        if( abs( $x_a - $x_b ) > 100 ) {
            # nothing is currently larger than diameter 100 / radius 50 XXX
            # the left object and right (which may not be right next to each other in the X buffer) are too far apart;
            # rather than advancing which object is the right object, keep that one, but advance the left one
            $left_i++;
            $right_i = $left_i;
            goto advanced_left_position;
        }

        # not interested in missile-missile collisions, and missiles are often near lots of other missiles
        next if ref $left eq 'missile' and ref $right eq 'missile';

        # co-op!
        next if ref $left eq 'missile' and ref $right eq 'ship';
        next if ref $left eq 'ship' and ref $right eq 'missile';

        # okay, ignoring asteroid-asteroid interactions for now, as much fun as that is
        next if ref $left eq 'asteroid' and ref $right eq 'asteroid';

        my $y_a = $left->[3]; # $left->y; ... speed
        my $y_b = $right->[3]; # $right->y; ... speed
        $y_b -= $world_y if $y_a > $world_y - 100 and $y_b < 100;  # $y_a is on the bottom edge and $y_a the top
        $y_a -= $world_y if $y_b > $world_y - 100 and $y_a < 100;  # $y_a is on the top edge and $y_a the bottom
        next unless abs( $y_a - $y_b ) < 100;  # nothing is currently larger than diameter 100 / radius 50 XXX

        # oh, they're close!
        my $size = $left->size + $right->size;
        next unless abs( $x_a - $x_b ) ** 2 + abs( $y_a - $y_b ) ** 2 <= $size ** 2;

        # warn "combined size: $size hypot: " . sqrt( abs( $x_a - $x_b ) ** 2 + abs( $y_a - $y_b ) ** 2 ) . '; a ' . ref( $left ) . ' hit a ' . ref( $right ); 

        # awww, shit
        $left->hit( $right );
        $right->hit( $left );

    }

}

sub draw_stuff {
    my $ship = shift;
local $SIG{__DIE__} = sub { Carp::cluck @_; die @_ };

    my $buf = '';

    my $draw_stuff_inner = sub {
        my $ship = shift;
        my $i = 0;
        # skip stuff off the left of the screen; 600 = about half of 1024, the screen X size, plus half of 100, the max diameter of an object

        $i++ while $i < $#objects and $objects[$i]->[2] + 600 < $ship->[2];
        while( $i < $#objects and $objects[$i]->[2] < $ship->[2] + 600 ) {
            $buf .= $objects[$i]->draw( $ship ) if $objects[$i]->[3] + 500 > $ship->[3] and $objects[$i]->[3] < $ship->[3] + 500;
            $i++;
        }
    };

    # my $draw_stuff_inner = sub {
    #     # naive algorithm; works, but may be slower
    #     my $ship = shift;
    #         for my $ob ( @objects ) {
    #             $buf .= $ob->draw( $ship );
    #         }
    # };

    $buf .= chr(27).chr(96); # normal line XXX
    $draw_stuff_inner->( $ship );

    if( $ship->x < 1024 / 2 or $ship->x > $world_x - 1024 / 2 or $ship->y < 768 / 2 or $ship->y > $world_y - 768 / 2 ) {

        # peer around the edge of the world to see what we'll see after we wrap around

        my $x_margin = 1024 / 2 + 50;
        my $y_margin = 768 / 2 + 50;

        my $dummy_ship = ship->new;
        if( $ship->x < $x_margin ) {
            $dummy_ship->x = $ship->x + $world_x;
        } elsif( $ship->x > $world_x - $x_margin ) {
            $dummy_ship->x = $ship->x - $world_x;
        } else {
            $dummy_ship->x = $ship->x;
        }
        if( $ship->y < $y_margin ) {
            $dummy_ship->y = $ship->y + $world_y;
        } elsif( $ship->y > $world_y - $y_margin ) {
            $dummy_ship->y = $ship->y - $world_y;
        } else {
            $dummy_ship->y = $ship->y;
        }
        $buf .= chr(27).chr(97); # dashed line XXX
        $draw_stuff_inner->( $dummy_ship );
        $buf .= chr(27).chr(96); # normal line XXX

    }

    return $buf;
}

sub draw_line {
    my $x1 = int shift;  my $y1 = int shift;
    return '' if $x1 < 0 or $x1 > 1023 or $y1 < 0 or $y1 >= 760; # XXX what is the bottom of the screen?
    my $x2 = int shift;  my $y2 = int shift;
    return '' if $x2 < 0 or $x2 > 1023 or $y2 < 0 or $y2 >= 760;
    my $tek_buffer = '';
    $tek_buffer .= chr(29);
    $tek_buffer .= draw_point($x1, $y1);
    $tek_buffer .= draw_point($x2, $y2);
    return $tek_buffer;
    # sleep 1;
}

sub draw_point {
    my $x = shift;
    my $y = 768 - shift;
    return chr(32 + (($y>>5)&0b011111)). chr(96 + ($y&0b011111)). chr(32 + (($x>>5)&0b011111)). chr(64 + ($x&0b011111));
}

sub text_mode {
    return join( '',
        draw_point(0, 0),                             # needed to clear screen
        chr(15+16),                   # text mode
    );
}

sub redraw_screen {
    my $message = shift;
    return join( '',
        chr(27), chr(12),   # clear screen 
    
        chr(13+16),                   # graphics mode   # needed to clear screen
        # draw_point(0, 738),         # position cursor                # also needed to clear screen for some reason
        draw_point(0, 6),             # position cursor                # also needed to clear screen for some reason
        chr(15+16),                   # text mode
        $message, "\n",

        chr(13+16),         # graphics mode
    );
}

#
#
#

package adrift;

sub rot :lvalue          { $_[0]->[0] }
sub rot_velocity :lvalue { $_[0]->[1] }
sub x :lvalue            { $_[0]->[2] }
sub y :lvalue            { $_[0]->[3] }
sub x_velocity :lvalue   { $_[0]->[4] }
sub y_velocity :lvalue   { $_[0]->[5] }
sub shape :lvalue        { $_[0]->[6] }       # array of alternating values for arc and radius
sub decay :lvalue        { $_[0]->[7] }
sub size :lvalue         { $_[0]->[8] }
sub destroyed :lvalue    { $_[0]->[9] }
sub invincible :lvalue   { $_[0]->[10] }

sub animate {
    my $self = shift;
    # $self->x += $self->x_velocity;   $self->x = $world_x if $self->x < 0; $self->x = 0 if $self->x > $world_x;
    # $self->y += $self->y_velocity;   $self->y = $world_y if $self->y < 0; $self->y = 0 if $self->y > $world_y;
    $self->[2] += $self->[4];   $self->[2] = $world_x if $self->[2] < 0; $self->[2] = 0 if $self->[2] > $world_x;
    $self->[3] += $self->[5];   $self->[3] = $world_y if $self->[3] < 0; $self->[3] = 0 if $self->[3] > $world_y;
}

sub draw {
    my $self = shift;
    my $viewer = shift;

    my $shape = $self->shape;
    my $x = $self->x;
    my $y = $self->y;

    my @shape = @$shape;  # copy
    my $arc = $self->rot;
    my $last_x;
    my $last_y;
    my $out = '';

    while( @shape ) {
        $arc += shift @shape;
        my $radius = shift @shape;
        my $pointX = $x + cos(Math::Trig::deg2rad($arc)) * $radius;
        my $pointY = $y + sin(Math::Trig::deg2rad($arc)) * $radius;
        $pointX = $pointX - $viewer->x + 1024/2;
        $pointY = $pointY - $viewer->y + 760/2;
        if( $last_x and $last_y ) {
            $out .= main::draw_line( $last_x, $last_y, $pointX, $pointY );
        }
        $last_x = $pointX; $last_y = $pointY;
    }

    return $out;
}

sub add_thrust {
    my $self = shift;
    my $thrust = shift;

    my $rot = $self->rot;

    my $new_x_thrust = cos(Math::Trig::deg2rad($rot)) * $thrust;
    my $new_y_thrust = sin(Math::Trig::deg2rad($rot)) * $thrust;

    $self->x_velocity += $new_x_thrust;
    $self->y_velocity += $new_y_thrust;
}

sub copy_position_and_velocity_to {
    my $self = shift;
    my $target = shift;
    $target->rot = $self->rot;
    $target->x = $self->x;
    $target->y = $self->y;
    $target->x_velocity = $self->x_velocity;
    $target->y_velocity = $self->y_velocity;
    return;
}

#
#
#

package asteroid;

use base 'adrift';

sub new {
    my $package = shift;
    my $size = shift() || 30;
    my $self = bless [ ], $package;
    $self->rot = int rand 360;
    $self->rot_velocity =  0.5 + rand 3.5; 
    $self->rot_velocity = - $self->rot_velocity if int rand 2;
    $self->x = int rand $world_x;
    $self->y = int rand $world_y;
    $self->x_velocity = (int rand 2) ? 1 + int rand 3 : - ( 1 + int rand 3);
    $self->y_velocity = (int rand 2) ? 1 + int rand 3 : - ( 1 + int rand 3);
    # $self->invincible = 20; # from each other, if enabled
    # my $i = int rand 40; # see note below about adding up all of the arcs to compute the final point
    my $i = 0;  # some random rotation should mask this lack of randomness
    my $flip_flop = 0;
    my $starting_radius;
    my $total_radius;  my $num_sections;  # for computing size
    my @shape;
    while( 1 ) {
        $i = 360 if $i > 360;
        $flip_flop = ! $flip_flop;
        my $radius = $flip_flop ? $size/2+int(rand $size) : $size+int(rand $size);
        $total_radius += $radius;  $num_sections++;  # for computing size
        $starting_radius ||= $radius;
        $radius = $starting_radius if $i >= 360;
        my $arc = int(rand 20)+int(rand 20)+int(rand 20);
        push @shape, $arc, $radius;
        last if $i >= 360;
        $i += $arc;
    }
    # push @shape, $shape[0];  # last point is the first point... except they aren't actually points; we'd have to add up all of the arcs to compute the last one, and then steal the radius from the first one, which we could do
    $self->shape = \@shape;
    $self->size = int( $total_radius / $num_sections );
    return $self;
}

sub animate {
    my $self = shift;
    # $self->SUPER::animate; # inline stuff instead
    $self->[2] += $self->[4];  $self->[2] = $world_x if $self->[2] < 0; $self->[2] = 0 if $self->[2] > $world_x;
    $self->[3] += $self->[5];  $self->[3] = $world_y if $self->[3] < 0; $self->[3] = 0 if $self->[3] > $world_y;
    # $self->invincible-- if $self->invincible > 0;
    $self->rot += $self->rot_velocity;
    $self->rot -= 360 if $self->rot >= 360;
}

sub hit {
    my $self = shift;
    my $them = shift;
    if( ref($them) eq 'asteroid' ) {
        return;
    }
    if( ref($them) eq 'ship' ) {
        $them->destroyed = $self unless $them->destroyed; # XXX
    }
    if( ref($them) eq 'missile' ) {
        $self->break_up;
    }
}

sub break_up {
    my $self = shift;

    my $x_delta = $self->x_velocity;
    my $y_delta = $self->y_velocity;
    my $radius = sqrt( abs($x_delta) ** 2 + abs($y_delta) ** 2 );
 
    $self->destroyed = 1;

    my $size = $self->size;
    return if $size / 2 < 6;

    my $num_new_asteroids = $size > 20 ? 3 : 2;

    for (1..$num_new_asteroids) {

        # my $asteroid = asteroid->new( $size * 2 / 3 ); # looks better but explodes exponentially into black ink when asteroids are allowed to hit each other
        my $asteroid = asteroid->new( $size / 2 );

        $self->copy_position_and_velocity_to( $asteroid );

        # $asteroid->invincible = 20;

        my $angle = int rand 360;

        $asteroid->x_velocity = cos(Math::Trig::deg2rad($angle)) * $radius;
        $asteroid->y_velocity = sin(Math::Trig::deg2rad($angle)) * $radius;

        $asteroid->x += cos(Math::Trig::deg2rad($angle)) * $size; # move it to the edge of us
        $asteroid->y += sin(Math::Trig::deg2rad($angle)) * $size; # move it to the edge

        # $asteroid->animate for 1..2; # try to get out of collision range before we make a huge mess XXX

        push @objects, $asteroid;
    }

}

#
#
#

package ship;

use base 'adrift';

sub new {
    my $package = shift;
    my $self = bless [ ], $package;
    # $self->shape = [  
    #     0,  3,  # back divit
    #     40,  8,  # fin
    #     140, 10, # nose
    #     140, 8,  # fin
    #     40, 3,   # back around again to the back divit
    # ];
    $self->shape = [  
        0, 10,   # nose
        140, 8,  # top back fin
        40,  3,  # back
        40,  8,  # bottom back fin
        140, 10, # back around to the nose
    ];
    # $self->x = 1024/2;
    # $self->y = 760/2;
    $self->x = int rand $world_x;
    $self->y = int rand $world_y;
    $self->x_velocity = 0;
    $self->y_velocity = 0;
    $self->size = 9;
    return $self;
}

sub animate {
    my $self = shift;
    # $self->SUPER::animate; # inline stuff instead
    $self->[2] += $self->[4];  $self->[2] = $world_x if $self->[2] < 0; $self->[2] = 0 if $self->[2] > $world_x;
    $self->[3] += $self->[5];  $self->[3] = $world_y if $self->[3] < 0; $self->[3] = 0 if $self->[3] > $world_y;
    #if( $self->x_velocity > 3 or $self->y_velocity > 3 ) {
    #    $self->x_velocity *= 0.95;
    #    $self->y_velocity *= 0.95;
    #}
}

sub hit { }

#
#
#

package missile;

use base 'adrift';

sub new {
    my $package = shift;
    my $self = bless [ ], $package;
    $self->decay = 300;
    $self->size = 1;
    return $self;
}

sub animate {
    my $self = shift;
    # $self->SUPER::animate; # inline stuff instead
    $self->[2] += $self->[4];  $self->[2] = $world_x if $self->[2] < 0; $self->[2] = 0 if $self->[2] > $world_x;
    $self->[3] += $self->[5];  $self->[3] = $world_y if $self->[3] < 0; $self->[3] = 0 if $self->[3] > $world_y;
    $self->destroyed = 1 if $self->decay-- <= 0;
}

sub draw {
    my $self = shift;
    my $viewer = shift;

    my $x = $self->x;
    my $y = $self->y;
    my $rot = $self->rot;

    my $last_x;
    my $last_y;

    my $x1 =    $x - $viewer->x + 1024/2;   # object - viewer is correct
    my $y1 =    $y - $viewer->y + 760/2;

    return main::draw_line(
        $x1, $y1, 
        $x1 + cos(Math::Trig::deg2rad($rot)) * 2, $y1 + sin(Math::Trig::deg2rad($rot)) * 2,
        # $x1 + 5, $y1 + 5,
    );
}

sub hit { }

#
#
#

package star;

use base 'adrift';

sub new {
    my $package = shift;
    my $self = bless [ ], $package;
    $self->x = int rand $world_x;
    $self->y = int rand $world_y;
    $self->size = 1;
    return $self;
}

sub hit { }

sub animate { }

sub draw {
    my $self = shift;
    my $viewer = shift;

    my $x =    $self->x - $viewer->x + 1024/2;
    my $y =    $self->y - $viewer->y + 760/2;
    my $radius = 3;

    my $buf = '';

    for my $arc ( 0, 45, 90 ) {
        my $x1 = $x + cos(Math::Trig::deg2rad($arc)) * $radius;
        my $y1 = $y + sin(Math::Trig::deg2rad($arc)) * $radius;
        my $x2 = $x + cos(180 + Math::Trig::deg2rad($arc)) * $radius;
        my $y2 = $y + sin(180 + Math::Trig::deg2rad($arc)) * $radius;
        $buf .= main::draw_line( $x1, $y1, $x2, $y2 );
    }

    return $buf;
}

__DATA__

in asteroid::hit:

        # XXX some random reforming would be pretty fun though... make one object 3/4 the mass of the two and another 1/4 the mass of the two, or make the proportion random and based on the relative sizes of the two things
        # XXX 1. if both heading on basically the same trajectory at the same speed, merge
        # XXX 2. if colloding and much larger, assimilate and add a random numbers to one or two of the radiuses in the shape
        # XXX 3. if colloding and the same side, bust up into a random number of smaller pieces
        # $self->break_up if ! $self->invincible;  # they'll do the same thing when his is called on them
        #if( abs( $them->size - $self->size ) < $self->size / 4  ) # close to the same size
        #    # holy god... in seconds, there were just a few giant asteroids
        #    my $shape = $self->shape;
        #    for( my $i = 1; $i < $#$shape; $i += 2 ) {
        #        $shape->[$i] += int rand( $shape->[$i] / 3);  # XXX or use the fraction of size they are of us
        #    }
        #    undef $them;
        #}

in asteroid::break_up:

    # remarkably, this doesn't seem to do much
    #$self->x_velocity= - $self->x_velocity;
    #$self->y_velocity= - $self->y_velocity;
    #$self->add_thrust( int rand 3 );  # this accelerates it in the direction it is facing; asteroids don't generally travel in their direction of facing; they just kind of spin and slide along
 
# XXXX
    # my $size = $self->size / 4;
    #for (1..4) {
    #    my $asteroid = asteroid->new( $size );
    #    $self->copy_position_and_velocity_to( $asteroid );
    #    $asteroid->rot += -90 + rand 180;
    #    # $asteroid->add_thrust( 1.5 );
    #    push @asteroids, $asteroid;
    #}
    # $self->destroyed = 1;

