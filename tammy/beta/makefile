# Find all source files, create a list of corresponding object files
PROGRAM = water_quality

EXT = f08
# Find all source files, create a list of corresponding object files
SRCS=$(wildcard *.$(EXT))
OBJS=$(patsubst %.$(EXT),%.o,$(SRCS))

# Ditto for mods (They will be in both lists)
MODS=$(wildcard mod*.$(EXT))
MOD_OBJS=$(patsubst %.$(EXT),%.o,$(MODS))

# Compiler/Linker settings
# gfortran - gnu (execution: ./a.out) (flags: gflags in ~/.bashrc)
# mpif90 - gnu, MPI Mac Desktop (execution: mpirun -np 4 ./a.out)
FC = ${fortran_compiler_mpi_gnu}
FLFLAGS = -g
FCFLAGS = -c ${gflags}
PRG_OBJ = $(PROGRAM).o

# Clean the suffixes
.SUFFIXES:

# Set the suffixes we are interested in
.SUFFIXES: .$(EXT) .o

# make without parameters will make first target found.
default : $(PROGRAM)

# Compiler steps for all objects
$(OBJS) : %.o : %.$(EXT)
	$(FC) $(FCFLAGS) -o $@ $<

# Linker
$(PROGRAM) : $(OBJS)
	$(FC) $(FLFLAGS) -o $@ $^

debug:
	@echo "${bold}PROGRAM${normal}  = $(PROGRAM)"
	@echo "${bold}PRG_OBJ${normal}  = $(PRG_OBJ)"
	@echo "${bold}SRCS${normal}     = $(SRCS)"
	@echo "${bold}OBJS${normal}     = $(OBJS)"
	@echo "${bold}MODS${normal}     = $(MODS)"
	@echo "${bold}MOD_OBJS${normal} = $(MOD_OBJS)"

clean:
	rm -rf $(OBJS) $(PROGRAM) $(patsubst %.o,%.mod,$(MOD_OBJS))
	rm -f *.mod *.smod *.o *.dSYM

.PHONY: debug default clean

# Dependencies

# Main program depends on all modules
$(PRG_OBJ) : $(MOD_OBJS)

# Module interdepencies
mod_allocator.o               : mod_set_precision.o mod_constants.o
mod_control.o                 : mod_set_precision.o
mod_constants.o               : mod_set_precision.o
mod_file_handling.o           :
mod_global_settings.o         :
mod_gridc.o                   : mod_set_precision.o
mod_io_handles.o              : mod_constants.o mod_global_settings.o mod_file_handling.o
mod_master.o                  : mod_gridc.o mod_read_input.o
mod_mpif_sgi.o                :
mod_read_input.o              : mod_set_precision.o mod_constants.o mod_file_handling.o 	mod_gridc.o
mod_set_precision.o           :
mod_time_stamp.o              :


# rditldmt@ITLDMT-MD-O2034:alpha $ date
# Thu Feb  9 14:37:34 CST 2017

# rditldmt@ITLDMT-MD-O2034:alpha $ pwd
# /Users/rditldmt/Documents/GitHub_Desktop/CE-QUAL-W2/tammy/alpha

# rditldmt@ITLDMT-MD-O2034:alpha $ make debug
# PROGRAM  = water_quality
# PRG_OBJ  = water_quality.o
# SRCS     = mod_allocator.f08 mod_constants.f08 mod_control.f08 mod_file_handling.f08 mod_global_settings.f08 mod_gridc.f08 mod_io_handles.f08 mod_master.f08 mod_mpif_sgi.f08 mod_read_input.f08 mod_set_precision.f08 mod_time_stamp.f08 water_quality.f08
# OBJS     = mod_allocator.o mod_constants.o mod_control.o mod_file_handling.o mod_global_settings.o mod_gridc.o mod_io_handles.o mod_master.o mod_mpif_sgi.o mod_read_input.o mod_set_precision.o mod_time_stamp.o water_quality.o
# MODS     = mod_allocator.f08 mod_constants.f08 mod_control.f08 mod_file_handling.f08 mod_global_settings.f08 mod_gridc.f08 mod_io_handles.f08 mod_master.f08 mod_mpif_sgi.f08 mod_read_input.f08 mod_set_precision.f08 mod_time_stamp.f08
# MOD_OBJS = mod_allocator.o mod_constants.o mod_control.o mod_file_handling.o mod_global_settings.o mod_gridc.o mod_io_handles.o mod_master.o mod_mpif_sgi.o mod_read_input.o mod_set_precision.o mod_time_stamp.o

# rditldmt@ITLDMT-MD-O2034:alpha $ echo $gflags
# -g -ffpe-trap=denormal,invalid,zero -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only

# rditldmt@ITLDMT-MD-O2034:alpha $ make
# mpif90 -c -g -ffpe-trap=denormal,invalid,zero -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_set_precision.o mod_set_precision.f08
# mpif90 -c -g -ffpe-trap=denormal,invalid,zero -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_constants.o mod_constants.f08
# mpif90 -c -g -ffpe-trap=denormal,invalid,zero -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_allocator.o mod_allocator.f08
# mpif90 -c -g -ffpe-trap=denormal,invalid,zero -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_control.o mod_control.f08
# mpif90 -c -g -ffpe-trap=denormal,invalid,zero -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_file_handling.o mod_file_handling.f08
# mpif90 -c -g -ffpe-trap=denormal,invalid,zero -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_global_settings.o mod_global_settings.f08
# mpif90 -c -g -ffpe-trap=denormal,invalid,zero -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_gridc.o mod_gridc.f08
# mpif90 -c -g -ffpe-trap=denormal,invalid,zero -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_io_handles.o mod_io_handles.f08
# mpif90 -c -g -ffpe-trap=denormal,invalid,zero -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_read_input.o mod_read_input.f08
# mpif90 -c -g -ffpe-trap=denormal,invalid,zero -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_master.o mod_master.f08
# mpif90 -c -g -ffpe-trap=denormal,invalid,zero -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_mpif_sgi.o mod_mpif_sgi.f08
# mpif90 -c -g -ffpe-trap=denormal,invalid,zero -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_time_stamp.o mod_time_stamp.f08
# mpif90 -c -g -ffpe-trap=denormal,invalid,zero -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o water_quality.o water_quality.f08
# mpif90 -g -o water_quality mod_allocator.o mod_constants.o mod_control.o mod_file_handling.o mod_global_settings.o mod_gridc.o mod_io_handles.o mod_master.o mod_mpif_sgi.o mod_read_input.o mod_set_precision.o mod_time_stamp.o water_quality.o
