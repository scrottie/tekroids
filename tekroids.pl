#!/usr/bin/perl

=for comment

o. decide on a size of the universe that's larger than the size of the screen
o. make asteroids go forever and wrap around the edges of the world
o. stick ships in random locations on the map and render each person's perspective
o. let people thrust around!
o. shoot stuff
o. maybe peoples ships should have varied shapes
o. 760 or 768?  get it right
o. asteroid gravitational attraction and merging?
o. right now, the ship can steer instantly; that's wrong; new moment needs to get added to existing
o. parallex stars?  starfield generator?

=cut

use strict;
use warnings;

use Coro;
use Coro::Socket;
# use Coro::Event;
# use Coro::AnyEvent;
use Coro::Event;
use Coro::Timer 'sleep';

use IO::Handle;
use IO::Socket;

# use POSIX qw(:errno_h);
# use Time::HiRes; # 'select';
use Time::HiRes 'usleep';
use Math::Trig 'deg2rad';
use Fcntl;

sub opt ($) { scalar grep $_ eq $_[0], @ARGV }
sub arg ($) { my $opt = shift; my $i=1; while($i<=$#ARGV) { return $ARGV[$i] if $ARGV[$i-1] eq $opt; $i++; } }

my $world_x = 8192;
my $world_y = 8192;
#my $world_x = 4096;
#my $world_y = 4096;

my @asteroids = map { asteroid->new } 1..200;
my @missiles;
my @ships;

my $port = 2003;

listen_again:
my $telnetlisten = eval { 
    warn "listening on port $port";
    Coro::Socket->new(
        LocalPort => $port,
        Type      => SOCK_STREAM,
        Listen    => SOMAXCONN,
    ); 
} or do { $port++; goto listen_again; };

async {

    # animate calls, collision detection

    while(1) {

        #
        # collision detection
        #
    
        my @all = ( @ships, @missiles, @asteroids );
    
        # for now, only interested in ship<->asteroid collisions and bullet<->asteroid collisions
    
        test_collided( @all );
    
        # for my $o ( @ships ) { undef $o if $o->destroyed }      @ships = grep defined, @ships;
        for my $o ( @asteroids ) { undef $o if $o->destroyed }  @asteroids = grep defined, @asteroids;

        #
        # move things
        #

        @asteroids = grep defined, @asteroids;

        for my $asteroid ( @asteroids ) {
            $asteroid->animate;
        }
 
        @ships = grep defined, @ships;
 
        for my $s (@ships) {
            $s->animate;
        }
 
        for my $missile (@missiles) {
            next unless $missile;
            $missile->animate or do { $missile = undef; next; }; # expired
        }
 
        @missiles = grep defined, @missiles;

        #
        #
        #
    
        Coro::Timer::sleep 1/60;

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
            # "\n",
        ); 

        my $line = '';      # character input buffer
        my $message = '';   # score / messages / diagnostic output to the player
        my $frame;

        my $ship = ship->new;
        push @ships, $ship;

      client_connected:
        while(1) {
    
            $client->print(
                chr(27), chr(12),   # clear screen 
    
                chr(13+16),                   # graphics mode   # needed to clear screen
                # draw_point(0, 738),         # position cursor                # also needed to clear screen for some reason
                draw_point(0, 6),             # position cursor                # also needed to clear screen for some reason
                chr(15+16),                   # text mode
                $message, "\n",
    
                chr(13+16),         # graphics mode
            );
    
            #
            # asteroids
            #
    
            if( $ship->destroyed ) {
                # only show the astroid that got us
                for my $asteroid ( @asteroids ) {
                    next unless defined $asteroid;
                    $client->print( $asteroid->draw( $ship ) ) if $ship->destroyed == $asteroid;
                }
            } else {
                for my $asteroid ( @asteroids ) {
                    next unless defined $asteroid;
                    $client->print( $asteroid->draw( $ship ) );
                }
            }
    
            #
            # ships
            #
    
            for my $s (@ships) {
                next unless defined $s;
                $client->print( $s->draw( $ship ) ) if ! $s->destroyed;
            }
    
            #
            # missiles 
            #
    
            for my $missile (@missiles) {
                next unless $missile;
                $client->print( $missile->draw( $ship ) );
            }
    
            #
            # input
            #
    
            $client->print(
                chr(27), chr(26),              # GIN... "escape-ENQ recieved from the computer while the crosshairs are being displayed..."... "escape SUB"
                chr(27), chr(5),               # escape-ENQ
            );
    
            my $line = '';
            while( length($line) < 4 ) {
                my $c = $client->getc();
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
                $thrust = 0.1 if $distance > 100;  # XXX tune this
                $thrust = 0.2 if $distance > 200;  # XXX tune this
                $thrust = 0.3 if $distance > 400;  # XXX tune this
    
                $ship->add_thrust( $thrust );
    
                $message = $ship->x . ' ' . $ship->y . ' ' . $ship->x_velocity . ' ' . $ship->y_velocity;
    
            }
    
            #
            # fire new missile
            #

            $frame++;
    
            if( ! $ship->destroyed ) {
                if( ! ( $frame % 7 ) ) {
                    my $missile = missile->new;
                    $ship->copy_position_and_velocity_to( $missile );
                    $missile->animate for 1..2; # start in front of the ship
                    $missile->add_thrust( 3 );
                    push @missiles, $missile;  
                }
            }
    
            # $message = 'ship x: ' . int($ship->x) . ' y: ' . int($ship->y) . ' missiles: ' . scalar(@missiles); $message .= ' x: ' . int($missiles[-1]->x) . ' y: ' . int($missiles[-1]->y) if @missiles; # XXX
    
            #
            #
            #
    
            # usleep 16666*2;  # 16666 = 1/60th of a second # XXX use AnyEvent::Timer instead
            Coro::Timer::sleep 1/60;
    
        }
    
        print "client went away: $!\n";

    }; # end client handling async block

} # end while 1

sub test_collided {

    my @all = sort { $a->x <=> $b->x } @_;

    my $left_i = 0;
    my $right_i = 0;

    # do an n:n comparison, comparing each object to each other one, inside of a sliding window of objects
    # whose x positions are within a hard-coded range of each other

    while(1) {

        $right_i++;

      advanced_left_position:

        next if $left_i == $right_i;

        last if $right_i > $#all;

        my $left = $all[ $left_i ];
        my $right = $all[ $right_i ];

        # print "test_collided: left_i: $left_i right_i: $right_i left x: @{[ $left->x ]} right x: @{[ $right->x ]}\n";

        my $x_a = $left->[2]; # $left->x; ... speed
        my $x_b = $right->[2]; # $right->x; ... speed

        if( abs( $x_a - $x_b ) > 200 ) {
            # nothing is currently larger than 100 XXX
            # the left object and right (which may not be right next to each other in the array) are too far apart;
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
        next unless abs( $y_a - $y_b ) < 200;  # nothing is currently larger than 100 XXX

        # oh, they're close!
        my $size = $left->size + $right->size;
        next unless sqrt( abs( $x_a - $x_b ) ** 2 + abs( $y_a - $y_b ) ** 2 ) <= $size;

        # die "combined size: $size hypot: " . sqrt( abs( $x_a - $x_b ) ** 2 + abs( $y_a - $y_b ) ** 2 ) . '; a ' . ref( $left ) . ' hit a ' . ref( $right ); 

        # awww, shit
        # $collided++; # XXX slow
        $left->hit( $right );
        $right->hit( $left );

    }

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
    $self->x += $self->x_velocity;   $self->x = $world_x if $self->x < 0; $self->x = 0 if $self->x > $world_x;
    $self->y += $self->y_velocity;   $self->y = $world_y if $self->y < 0; $self->y = 0 if $self->y > $world_y;
}

sub draw {
    my $self = shift;
    my $viewer = shift;

    my $shape = $self->shape;
    my $x = $self->x;
    my $y = $self->y;

    return '' if abs($x - $viewer->x) < -100 or abs($x - $viewer->x) > 1124 or abs($y - $viewer->y) < -100 or abs($y - $viewer->y) > 1124;

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
        # $pointX = $viewer->x - $pointX + 1024/2;
        # $pointY = $viewer->y - $pointY + 760/2;
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
    $self->invincible = 20; # from each other, if enabled
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
    $self->SUPER::animate;
    $self->invincible-- if $self->invincible > 0;
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
        $them->destroyed = $self;
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
    return if $size / 2 < 5;

    for (1..3) {

        # my $asteroid = asteroid->new( $size * 2 / 3 ); # looks better but explodes exponentially into black ink when asteroids are allowed to hit each other
        my $asteroid = asteroid->new( $size / 2 );

        $self->copy_position_and_velocity_to( $asteroid );

        $asteroid->invincible = 20;

        my $angle = int rand 360;

        $asteroid->x_velocity = cos(Math::Trig::deg2rad($angle)) * $radius;
        $asteroid->y_velocity = sin(Math::Trig::deg2rad($angle)) * $radius;

        $asteroid->x += cos(Math::Trig::deg2rad($angle)) * $size; # move it to the edge of us
        $asteroid->y += sin(Math::Trig::deg2rad($angle)) * $size; # move it to the edge

        # $asteroid->animate for 1..2; # try to get out of collision range before we make a huge mess XXX

        push @asteroids, $asteroid;
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
    $self->SUPER::animate;
    $self->x_velocity *= 0.95; # XXX tune this
    $self->y_velocity *= 0.95;
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
    $self->SUPER::animate;
    return 1 if $self->decay-- > 0;
}

sub draw {
    my $self = shift;
    my $viewer = shift;

    my $x = $self->x;
    my $y = $self->y;
    my $rot = $self->rot;

    return '' if abs($x - $viewer->x) < -100 or abs($x - $viewer->x) > 1124 or abs($y - $viewer->y) < -100 or abs($y - $viewer->y) > 1124;

    my $last_x;
    my $last_y;

    my $x1 =    $x - $viewer->x + 1024/2;
    my $y1 =    $y - $viewer->y + 760/2;

    return main::draw_line(
        $x1, $y1, 
        $x1 + cos(Math::Trig::deg2rad($rot)) * 2, $y1 + sin(Math::Trig::deg2rad($rot)) * 2,
        # $x1 + 5, $y1 + 5,
    );
}

sub hit { }

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

