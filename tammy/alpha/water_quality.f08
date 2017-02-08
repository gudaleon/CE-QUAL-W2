! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 23456789 023456789 123456789 223456789 32
program water_quality

    use, intrinsic :: iso_fortran_env,  only : compiler_version, compiler_options, stdout => output_unit

    !use mFileHandling,                  only : safeopen_writereplace
    !use mMPI_SGI,                       only : mpi_comm_world, mpi_wtime, mpi_char, mpi_double, mpi_long, mpi_max, mpi_min  ! SGI
    !use mSetPrecision,                  only : ip, rp

    implicit none

        ! test cases


        write ( stdout, '( /, "Fortran compiler version:    ", g0    )' ) compiler_version ( )
        write ( stdout, '(    "Fortran compilation options: ", g0, / )' ) compiler_options ( )

    stop 'execution completed for program water_quality'

end program water_quality

! rditldmt@ITLDMT-MD-O2034:charlie $ date
! Thu Jan  5 17:36:03 CST 2017
! rditldmt@ITLDMT-MD-O2034:charlie $ pwd
! /Users/rditldmt/hpc/fortran/RA/el/w2/charlie
! rditldmt@ITLDMT-MD-O2034:charlie $ make
! mpif90 -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_set_precision.o mod_set_precision.f08
! mpif90 -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_constants.o mod_constants.f08
! mpif90 -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_file_handling.o mod_file_handling.f08
! mpif90 -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_mpif_sgi.o mod_mpif_sgi.f08
! mpif90 -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o water_quality.o water_quality.f08
! mpif90 -g -o water_quality mod_constants.o mod_file_handling.o mod_mpif_sgi.o mod_set_precision.o water_quality.o
! rditldmt@ITLDMT-MD-O2034:charlie $ ./water_quality
! rditldmt@ITLDMT-MD-O2034:charlie $ ./water_quality
!
! Fortran compiler version:    GCC version 5.4.0
! Fortran compilation options: -I /opt/local/include/mpich-gcc5 -I /opt/local/include/mpich-gcc5 -fPIC -feliminate-unused-debug-symbols -mmacosx-version-min=10.11.6 -m64 -mtune=core2 -auxbase-strip water_quality.o -g -Og -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wpedantic -Wuse-without-only -ffpe-trap=denormal -fbacktrace -fcheck=bounds -fmax-errors=5
!
! STOP execution completed for program water_quality
