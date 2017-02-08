! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
module mControl

    use mSetPrecision,                  only : ip, rp

    implicit none

    type :: control
        ! rank 3
        real ( rp ),    allocatable, dimension ( : , : , : ) :: C2, CD
        ! rank 2
        ! rank 1
        integer ( ip ), allocatable, dimension ( : )         :: SSAC, ALAC
        ! rank 0
        integer ( ip )                                       :: IMX, KMX, UNIT, NERR, NWRN, NASS, NALL, NBOD
        logical                                              :: DELETE_ERR, DELETE_WRN
    ! contains
    !     private
    !     procedure, public :: allocator      =>  allocator_sub
    end type control
! contains
end module mControl
