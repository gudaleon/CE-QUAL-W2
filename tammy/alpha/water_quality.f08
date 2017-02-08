! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 23456789 023456789 123456789 223456789 32
program water_quality

    use, intrinsic :: iso_fortran_env,  only : compiler_version, compiler_options, stdout => output_unit

    use mConstants,                     only : zero, stdout, fmt_generic
    !use mFileHandling,                  only : safeopen_writereplace
    !use mMPI_SGI,                       only : mpi_comm_world, mpi_wtime, mpi_char, mpi_double, mpi_long, mpi_max, mpi_min  ! SGI
    use mMaster,                        only : develop
    use mSetPrecision,                  only : rp
    use mTimeStamp,                     only : timestamp

    implicit none

    real ( rp ) :: cpu_time_start = zero, cpu_time_stop = zero, cpu_time_elapsed = zero

        ! test cases
        call cpu_time ( cpu_time_start )

            write ( *, "( /, 'Running CE-QUAL-W2 ...' )" )
            call develop ( )

        call cpu_time ( cpu_time_stop  )
        cpu_time_elapsed = cpu_time_stop - cpu_time_start

        write ( stdout, * )
        write ( stdout, fmt_generic ) 'cpu seconds: ', cpu_time_elapsed
        write ( stdout, fmt_generic ) 'timestamp: ', timestamp ( )
        write ( stdout, * )
        write ( stdout, fmt_generic ) "Fortran compiler version:    ", compiler_version ( )
        write ( stdout, fmt_generic ) "Fortran compilation options: ", compiler_options ( )
        write ( stdout, * )

    stop 'execution completed for program water_quality'

end program water_quality

! rditldmt@ITLDMT-MD-O2034:alpha $ date
! Wed Feb  8 17:47:42 CST 2017

! rditldmt@ITLDMT-MD-O2034:alpha $ pwd
! /Users/rditldmt/Documents/GitHub_Desktop/CE-QUAL-W2/tammy/alpha

! rditldmt@ITLDMT-MD-O2034:alpha $ echo $gflags
! -g -ffpe-trap=denormal,invalid,zero -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only

! rditldmt@ITLDMT-MD-O2034:alpha $ make
! mpif90 -c -g -ffpe-trap=denormal,invalid,zero -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_set_precision.o mod_set_precision.f08
! mpif90 -c -g -ffpe-trap=denormal,invalid,zero -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_constants.o mod_constants.f08
! mpif90 -c -g -ffpe-trap=denormal,invalid,zero -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_allocator.o mod_allocator.f08
! mpif90 -c -g -ffpe-trap=denormal,invalid,zero -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_control.o mod_control.f08
! mpif90 -c -g -ffpe-trap=denormal,invalid,zero -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_file_handling.o mod_file_handling.f08
! mpif90 -c -g -ffpe-trap=denormal,invalid,zero -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_global_settings.o mod_global_settings.f08
! mpif90 -c -g -ffpe-trap=denormal,invalid,zero -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_gridc.o mod_gridc.f08
! mpif90 -c -g -ffpe-trap=denormal,invalid,zero -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_io_handles.o mod_io_handles.f08
! mpif90 -c -g -ffpe-trap=denormal,invalid,zero -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_read_input.o mod_read_input.f08
! mpif90 -c -g -ffpe-trap=denormal,invalid,zero -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_master.o mod_master.f08
! mpif90 -c -g -ffpe-trap=denormal,invalid,zero -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_mpif_sgi.o mod_mpif_sgi.f08
! mpif90 -c -g -ffpe-trap=denormal,invalid,zero -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_time_stamp.o mod_time_stamp.f08
! mpif90 -c -g -ffpe-trap=denormal,invalid,zero -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o water_quality.o water_quality.f08
! mpif90 -g -o water_quality mod_allocator.o mod_constants.o mod_control.o mod_file_handling.o mod_global_settings.o mod_gridc.o mod_io_handles.o mod_master.o mod_mpif_sgi.o mod_read_input.o mod_set_precision.o mod_time_stamp.o water_quality.o

! rditldmt@ITLDMT-MD-O2034:alpha $ ./water_quality
!
! Running CE-QUAL-W2 ...
! attempting to open ../test/w2_con.npt, ../test/pre.err, ../test/pre.wrn, ../test/pre.opt.
! files opened successfully
! Control file
!
! cpu seconds: 0.51200000000000030E-003
! timestamp: 2017-02-08  17:48:09  UCT-0600
!
! Fortran compiler version:    GCC version 6.3.0
! Fortran compilation options: -I /opt/local/include/mpich-mp -I /opt/local/include/mpich-mp -fPIC -feliminate-unused-debug-symbols -mmacosx-version-min=10.11.6 -mtune=core2 -auxbase-strip water_quality.o -g -Og -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wpedantic -Wuse-without-only -ffpe-trap=denormal,invalid,zero -fbacktrace -fcheck=bounds -fmax-errors=5
!
! STOP execution completed for program water_qualityrditldmt@ITLDMT-MD-O2034:alpha $ date
! Wed Feb  8 17:47:42 CST 2017
! rditldmt@ITLDMT-MD-O2034:alpha $ pwd
! /Users/rditldmt/Documents/GitHub_Desktop/CE-QUAL-W2/tammy/alpha
! rditldmt@ITLDMT-MD-O2034:alpha $ echo $gflags
! -g -ffpe-trap=denormal,invalid,zero -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only
! rditldmt@ITLDMT-MD-O2034:alpha $ make
! mpif90 -c -g -ffpe-trap=denormal,invalid,zero -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_set_precision.o mod_set_precision.f08
! mpif90 -c -g -ffpe-trap=denormal,invalid,zero -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_constants.o mod_constants.f08
! mpif90 -c -g -ffpe-trap=denormal,invalid,zero -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_allocator.o mod_allocator.f08
! mpif90 -c -g -ffpe-trap=denormal,invalid,zero -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_control.o mod_control.f08
! mpif90 -c -g -ffpe-trap=denormal,invalid,zero -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_file_handling.o mod_file_handling.f08
! mpif90 -c -g -ffpe-trap=denormal,invalid,zero -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_global_settings.o mod_global_settings.f08
! mpif90 -c -g -ffpe-trap=denormal,invalid,zero -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_gridc.o mod_gridc.f08
! mpif90 -c -g -ffpe-trap=denormal,invalid,zero -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_io_handles.o mod_io_handles.f08
! mpif90 -c -g -ffpe-trap=denormal,invalid,zero -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_read_input.o mod_read_input.f08
! mpif90 -c -g -ffpe-trap=denormal,invalid,zero -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_master.o mod_master.f08
! mpif90 -c -g -ffpe-trap=denormal,invalid,zero -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_mpif_sgi.o mod_mpif_sgi.f08
! mpif90 -c -g -ffpe-trap=denormal,invalid,zero -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_time_stamp.o mod_time_stamp.f08
! mpif90 -c -g -ffpe-trap=denormal,invalid,zero -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o water_quality.o water_quality.f08
! mpif90 -g -o water_quality mod_allocator.o mod_constants.o mod_control.o mod_file_handling.o mod_global_settings.o mod_gridc.o mod_io_handles.o mod_master.o mod_mpif_sgi.o mod_read_input.o mod_set_precision.o mod_time_stamp.o water_quality.o

! rditldmt@ITLDMT-MD-O2034:alpha $ ./water_quality
!
! Running CE-QUAL-W2 ...
! attempting to open ../test/w2_con.npt, ../test/pre.err, ../test/pre.wrn, ../test/pre.opt.
! files opened successfully
! Control file
!
! cpu seconds: 0.51200000000000030E-003
! timestamp: 2017-02-08  17:48:09  UCT-0600
!
! Fortran compiler version:    GCC version 6.3.0
! Fortran compilation options: -I /opt/local/include/mpich-mp -I /opt/local/include/mpich-mp -fPIC -feliminate-unused-debug-symbols -mmacosx-version-min=10.11.6 -mtune=core2 -auxbase-strip water_quality.o -g -Og -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wpedantic -Wuse-without-only -ffpe-trap=denormal,invalid,zero -fbacktrace -fcheck=bounds -fmax-errors=5
!
! STOP execution completed for program water_quality
