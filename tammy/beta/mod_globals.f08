module mGlobals

    use mSetPrecision,                  only : ip, sp, rp

    implicit none

    ! Reals
    ! queezy about single precision
    real ( sp ), allocatable, dimension ( : , : , : , : ) :: AF, EF
    real ( sp ), allocatable, dimension ( : , : , : )     :: KF, CD, ALLIM, APLIM, ANLIM, ASLIM, KFS, ELLIM, EPLIM, ENLIM, ESLIM
    real ( rp ), allocatable, dimension ( : , : , : )     :: C1, C2, C1S, CSSB, CSSK, HYD
    real ( rp ), allocatable, dimension ( : , : )         :: T1, TSS, QSS, VOLUH2, VOLDH2, QUH1, QDH1, UXBR, UYBR, VOL, &
                                                             RATZ, CURZ1, CURZ2, CURZ3    ! SW 5/15/06
    real ( rp ), pointer,     dimension ( : , : )         :: U, W, T2, AZ, RHO, ST, SB, DLTLIM, VSH, ADMX, DM, ADMZ, HDG, HPG, GRAV
    real ( sp ), allocatable, dimension ( : )             :: TN_SEDSOD_NH4, TP_SEDSOD_PO4, TPOUT, TPTRIB, TPDTRIB, TPWD, TPPR, &
                                                             TPIN, TNOUT, TNTRIB, TNDTRIB, TNWD, TNPR, TNIN   !TP_SEDBURIAL,TN_SEDBURIAL
    real ( rp ), allocatable, dimension ( : )             :: ICETH, ELKT, HMULT, CMULT, CDMULT, WIND2, AZMAX, PALT, Z0
    real ( rp )                                           :: DENSITY, DLT, DLTMIN, DLTTVD, BETABR, START, HMAX2, CURRENT

    ! Integers
    integer ( ip ), allocatable, dimension ( : , : ) :: OPT
    integer ( ip ), allocatable, dimension ( : )     :: BS, BE, US, CUS, DS, JBDN, KB, KTI, SKTI, KTWB, KBMIN, CDHS, &
                                                        UHS, DHS, UQB, DQB
    integer ( ip ), pointer,     dimension ( : )     :: SNP, PRF, VPL, CPL, SPR, FLX, FLX2

    logical, allocatable, dimension ( : ) :: ICE, ICE_CALC, LAYERCHANGE

    character ( len =  10 ) :: CCTIME
    character ( len =  12 ) :: CDATE
    character ( len =  72 ) :: RSIFN
    character ( len =  10 ) :: MODDIR                     ! CURRENT WORKING DIRECTORY

end module mGlobals
