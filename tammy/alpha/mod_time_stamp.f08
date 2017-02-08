! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
module mTimeStamp

    implicit none

contains

    function timestamp ( ) result ( now ) ! 2013-06-06  19:47:03  UCT-0600

        integer, dimension ( 8 ) :: values  ! DTG ( date time group )

        character ( kind = kind ( 'A' ), len = 30 ) :: now  ! nice format
        character ( kind = kind ( 'A' ), len =  8 ) :: date ! ccyymmdd
        character ( kind = kind ( 'A' ), len = 10 ) :: time ! hhmmss.sss
        character ( kind = kind ( 'A' ), len =  5 ) :: zone ! (+-)hhmm, difference from Coordinated Universal Time (UTC)

            ! timestamp
            call date_and_time ( date = date, time = time, zone = zone, values = values )

            write  ( now, 100 )  date ( 1 : 4 ), date ( 5 : 6 ), date ( 7 : 8 ), &
                                 time ( 1 : 2 ), time ( 3 : 4 ), time ( 5 : 6 ), "UCT", zone
        100 format ( a, "-", a, "-", a, 2X, a, ":", a, ":", a, 2X, a, a )

    end function timestamp

end module mTimeStamp
