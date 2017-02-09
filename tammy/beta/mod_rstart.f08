! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
module mRStart

    use mSetPrecision,                  only : ip, rp

    implicit none

    type :: rstart
        ! Reals
        ! rank 3
        real ( rp ), allocatable, dimension ( : , : , : ) :: CSSUH2, CSSDH2
        ! rank 2
        real ( rp ), allocatable, dimension ( : , : )     :: TSSUH2, TSSDH2, SAVH2, SAVHR, SU, SW, SAZ, CMBRT
        ! rank 1
        real ( rp ), allocatable, dimension ( : )         :: NXTMSN, NXTMPR, NXTMSP, NXTMCP, NXTMVP, NXTMSC, NXTMFL,               & ! promoted to rp
                                                             SBKT,   ELTMF,                                                        &
                                                             VOLIN,  VOLOUT, VOLUH,  VOLDH,  VOLPR,  VOLTRB, VOLDT, VOLWD, VOLEV,  &
                                                             VOLICE, ICEBANK,                                                      &
                                                             VOLSBR, VOLTBR, VOLSR,  VOLTR,                                        &
                                                             TSSEV,  TSSPR,  TSSTR,  TSSDT,  TSSWD,  TSSUH,  TSSDH, TSSIN, TSSOUT, &
                                                             TSSS,   TSSB,   TSSICE,                                               &
                                                             ESBR,   ETBR,   EBRI,   SZ
        ! rank 0
        real ( rp )                                       :: ELTM,   NXTMRS, NXTMWD, NXTMTS

        ! Integers
        integer ( ip ), allocatable, dimension ( : ) :: SNPDP, VPLDP,  CPLDP, PRFDP,  SCRDP, SPRDP, FLXDP, NSPRF
        integer ( ip )                               :: DLTS,  CURMAX, DLTFF, DLTMAXX    ! SW 7/13/2010
    ! contains
    !     private
    !     procedure, public :: mySub      =>  mySub_sub
    end type rstart
! contains
end module mRStart
