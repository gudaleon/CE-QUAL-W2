! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32

!***********************************************************************************************************************************
!*                                                     Task 1:  Read all inputs                                                   **
!***********************************************************************************************************************************

module mReadInput

    !use mAllocator,                     only : allocate_rank_1_char_sub
    use mConstants,                     only : stdout, fmt_generic, fmt_err_read, error_fatal
    use mFileHandling,                  only : find_IU_info
    use mIOHandles,                     only : io_handles
    use mGridC,                         only : gridc
    use mSetPrecision,                  only : ip

    implicit none

    character ( len =   8 ) :: AID

    integer                 :: io_status
    character ( len = 256 ) :: io_msg

contains

    subroutine read_con_sub ( myGrid, myIO )

        ! slot variables
        type ( gridc ),      intent ( inout ) :: myGrid
        type ( io_handles ), intent ( in )    :: myIO
        ! local variables
        integer :: j

            ! Title cards
            write ( stdout, fmt_generic ) 'Control file'
            call myGrid % allocate_title ( )
            !call allocate_rank_1_char_sub ( myGrid % TITLE, 11_ip )

            write ( stdout, fmt_generic ) '  title cards'
            read  ( myIO % con ,'( // A8 / ( 8X, A72 ) )', iostat = io_status, iomsg = io_msg ) AID, &
                                                                                    ( myGrid % TITLE ( j ), j = 1, 10 )
            if ( io_status /= 0 ) then
                write ( stdout, fmt_err_read ) 'READ', io_status, trim ( io_msg )
                call find_IU_info ( myIO % con )
                stop error_fatal
            end if

            write ( stdout, fmt_generic )'fmt_err_read = ', fmt_err_read
        ! IF (AID /= 'TITLE C ')               GO TO 400
        !
        ! ! Array dimensions
        !
        ! write ( stdout, fmt_generic ) '  array dimensions'
        ! READ (CON,'(/A8/(8X,5I8,A8))')       AID, NWB, NBR, IMX, KMX, NPROC, CLOSEC
        ! IF (AID /= 'GRID    ')               GO TO 400
        ! READ (CON,'(/A8/(8X,10I8))')         AID, NTR, NST, NIW, NWD,  NGT, NSP, NPI, NPU
        ! IF (AID /= 'IN/OUTFL')               GO TO 400
        ! READ (CON,'(/A8/(8X,10I8))')         AID, NGC, NSS, NAL, NEP,  NBOD, NMC, NZP
        ! IF (AID /= 'CONSTITU')               GO TO 400
        ! READ (CON,'(/A8/(8X,I8,6a8))')         AID, NOD,SELECTC,HABTATC,ENVIRPC,AERATEC,INITUWL
        ! IF (AID /= 'MISCELL ')               GO TO 400

    end subroutine read_con_sub

end module mReadInput
