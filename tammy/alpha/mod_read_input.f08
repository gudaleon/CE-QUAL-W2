! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32

!***********************************************************************************************************************************
!*                                                     Task 1:  Read all inputs                                                   **
!***********************************************************************************************************************************

module mReadInput

    use mAllocator,                     only : allocate_rank_1_char_sub
    use mConstants,                     only : stdout, fmt_generic
    use mIOHandles,                     only : io_handles
    use mGridC,                         only : gridc
    use mSetPrecision,                  only : ip

    implicit none

contains

    subroutine read_con_sub ( myGrid )

        type ( gridc ), intent ( inout ) :: myGrid

        ! Title cards
        write ( stdout, fmt_generic ) 'Control file'
        call allocate_rank_1_char_sub ( myGrid % TITLE, 11_ip )
        ! write ( stdout, fmt_generic ) '  title cards'
        ! read (CON,'(//A8/(8X,A72))',ERR=400) AID, (TITLE(J),J=1,10)
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
