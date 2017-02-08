module mSetPrecision

    use, intrinsic :: iso_fortran_env, only : INT8, INT16, INT32, INT64, REAL32, REAL64, REAL128

    implicit none

    ! Define working precision: Hansen and Tompkins, p. 22
    integer, parameter :: rp = REAL64
    integer, parameter :: ip = INT64

end module mSetPrecision
