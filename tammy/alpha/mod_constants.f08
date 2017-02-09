! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
module mConstants

    use, intrinsic :: iso_fortran_env,  only : stdin => input_unit, stdout => output_unit

    use mSetPrecision, only : rp

    implicit none

    ! precision controlled numbers
    real ( rp ), parameter :: zero = 0.0_rp, one = 1.0_rp, two = 2.0_rp, three = 3.0_rp
    real ( rp ), parameter :: half = one / two

    real ( rp ), parameter :: pi = acos ( - 1.0_rp )

    ! formats
    character ( len = * ), parameter :: fmt_generic  = '( * ( g0 ) )'
    character ( len = * ), parameter :: fmt_err_read = '( "I/O failure during ", g0, /, "iostatus = ", g0,' // &
                                                       ' /, "iomsg = ", g0, ".", / )'

    ! messages
    character ( len = * ), parameter :: error_fatal = 'Halting on fatal error . . .'

end module mConstants
