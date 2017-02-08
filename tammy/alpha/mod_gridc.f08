! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
module mGridC

    use mSetPrecision,                  only : ip, rp

    implicit none

    type :: gridc
        ! real
        ! rank 2
        real ( rp ), allocatable, dimension ( : , : ) :: H, EL
        ! rank 1
        real ( rp ), pointer,     dimension ( : )     :: T2
        real ( rp ), allocatable, dimension ( : )     :: Z, XF, CMULT, DMULT, HMULT, CDMULT, ICETH, ALPHA, &
                                                         CMIN, CMAX, HYMIN, HYMAX, CDMIN, CDMAX
        ! integer
        ! rank 2
        integer ( ip ), allocatable, dimension ( : , : ) :: IPR, CDN
        ! rank 1
        integer ( ip ), allocatable, dimension ( : )     :: IEPR, KEPR, NADC, NACTR, NACIN, NACPR, NACDT, &
                                                            KTWB, KB, CUS, US, DS, BS, BE,                &
                                                            CN, LNAME
        ! rank 0
        integer ( ip )                                   :: JW, NAC, INI = 10

        ! rank 1
        logical,        allocatable, dimension ( : )     :: ICE_CALC

        ! character
        ! rank 2
        character ( len =  8 ), allocatable, dimension ( : , : ) :: HPRWBC, CDWBC, CPRWBC
        character ( len = 10 ), allocatable, dimension ( : , : ) :: CONV
        ! rank 1
        character ( len =  6 ), allocatable, dimension ( : )     :: CUNIT, CUNIT2
        character ( len =  8 ), allocatable, dimension ( : )     :: CDNAME2
        character ( len =  9 ), allocatable, dimension ( : )     :: CDNAFMTH, FMTC, FMTCDME2
        character ( len = 19 ), allocatable, dimension ( : )     :: CNAME1
        character ( len = 43 ), allocatable, dimension ( : )     :: CNAME, CDNAME, HNAME
        character ( len = 72 ), allocatable, dimension ( : )     :: TITLE

    end type gridc

end module mGridC
