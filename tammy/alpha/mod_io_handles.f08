! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
module mIOHandles

    use mConstants,                     only : stdout, fmt_generic
    use mFileHandling,                  only : safeopen_readonly, safeopen_writeappend
    use mGlobalSettings,                only : debug

    implicit none

    type :: io_handles

        integer :: con  ! control file
        integer :: cerr ! preprocessor error output
        integer :: wrn  ! preprocessor warning output
        integer :: cin  ! preprocessor output

    contains
        private
        procedure, public :: open_files => open_files_sub
    end type io_handles

contains

    subroutine open_files_sub ( me, CONFN, ERRFN, WRNFN, INIFN, path )

        class ( io_handles ), target                   :: me

        character ( len = * ), intent ( in )           :: CONFN, ERRFN, WRNFN, INIFN
        character ( len = * ), intent ( in ), optional :: path

        character ( len = 256 ) :: myPath

            if ( present ( path ) ) then
                myPath = path
            else
                myPath = ''
            end if


            if ( debug ) write ( stdout, fmt_generic ) 'attempting to open ',           &
                                                        trim ( myPath ) // CONFN, ', ', &
                                                        trim ( myPath ) // ERRFN, ', ', &
                                                        trim ( myPath ) // WRNFN, ', ', &
                                                        trim ( myPath ) // INIFN, '.'

            me % con  = safeopen_readonly    ( trim ( myPath ) // CONFN )
            me % cerr = safeopen_writeappend ( trim ( myPath ) // ERRFN )
            me % wrn  = safeopen_writeappend ( trim ( myPath ) // WRNFN )
            me % cin  = safeopen_writeappend ( trim ( myPath ) // INIFN )

            if ( debug ) write ( stdout, fmt_generic ) 'files opened successfully'

    end subroutine open_files_sub


end module mIOHandles
