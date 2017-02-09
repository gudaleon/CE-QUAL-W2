! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
module mParameters

    use mSetPrecision, only : rp, sp

    implicit none

    real ( sp ), parameter :: W2VER = 4.0_sp

    ! precision controlled numbers
    ! module global
    real ( rp ), parameter :: DAY = real ( 60 * 60 * 24, rp ) ! seconds in a day
    real ( rp ), parameter :: NONZERO = 1.0E-20_rp,    REFL  = 0.94_rp,   FRAZDZ = 0.14_rp, DZMIN = 1.4E-7_rp, &
                              AZMIN = DZMIN * 10.0_rp, DZMAX = 1000.0_rp, RHOW   = 1000.0_rp

end module mParameters
