module mGlobals ! w2modules.f90, module GLOBAL (line 91)

    use mConstants,                     only : zero
    use mSetPrecision,                  only : ip, sp, rp

    implicit none

    type :: global
        real ( rp ), allocatable, dimension ( : , : , : , : ) :: AF, EF
        real ( rp ), allocatable, dimension ( : , : , : )     :: KF, CD, ALLIM, APLIM, ANLIM, ASLIM, KFS, ELLIM, EPLIM, ENLIM, ESLIM
        real ( rp ), allocatable, dimension ( : , : , : )     :: C1, C2, C1S, CSSB, CSSK, HYD
        real ( rp ), allocatable, dimension ( : , : )         :: T1, TSS, QSS, VOLUH2, VOLDH2, QUH1, QDH1, UXBR, UYBR, VOL,        &
                                                                 RATZ, CURZ1, CURZ2, CURZ3    ! SW 5/15/06
        real ( rp ), pointer,     dimension ( : , : )         :: U, W, T2, AZ, RHO, ST, SB,                                        &
                                                                 DLTLIM, VSH, ADMX, DM, ADMZ, HDG, HPG, GRAV
        real ( rp ), allocatable, dimension ( : )             :: TN_SEDSOD_NH4, TP_SEDSOD_PO4, TPOUT, TPTRIB, TPDTRIB, TPWD, TPPR, &
                                                                 TPIN, TNOUT, TNTRIB, TNDTRIB, TNWD, TNPR, TNIN   !TP_SEDBURIAL,TN_SEDBURIAL
        real ( rp ), allocatable, dimension ( : )             :: ICETH, ELKT, HMULT, CMULT, CDMULT, WIND2, AZMAX, PALT, Z0
        real ( rp )                                           :: DENSITY, DLT, DLTMIN, DLTTVD, BETABR, START, HMAX2, CURRENT

        integer ( ip ), allocatable, dimension ( : , : ) :: OPT
        integer ( ip ), allocatable, dimension ( : )     :: BS, BE, US, CUS, DS, JBDN, KB, KTI, SKTI, KTWB, KBMIN, CDHS,           &
                                                            UHS, DHS, UQB, DQB
        integer ( ip ), pointer,     dimension ( : )     :: SNP, PRF, VPL, CPL, SPR, FLX, FLX2
        integer ( ip )                                   :: IMX, KMX, NBR, NTR, NWD, NWB, NCT, NBOD, NST, NSP, NGT, NPI, NPU, NWDO,&
                                                            NIKTSR, NUNIT, JW, JB, JC, IU, ID, KT, I, JJB, NOD, NDC, NAL, NSS, NHY,&
                                                            NFL, NEP, NEPT,                                                        &
                                                            NZP, NZPT, JZ, NZOOS, NZOOE, NMC, NMCT  ! number of zooplankton groups, CONSTIUENT NUMBER FOR ZOOPLANKTON, START AND END

        logical, allocatable, dimension ( : ) :: ICE, ICE_CALC, LAYERCHANGE

        character ( len =  10 ) :: CCTIME
        character ( len =  12 ) :: CDATE
        character ( len =  72 ) :: RSIFN
        character ( len = 180 ) :: MODDIR                     ! CURRENT WORKING DIRECTORY

    contains
        private
        procedure, public :: SetZero        => SetZero_sub
        procedure, public :: SetZeroRestart => SetZeroRestart_sub
    end type global

    private :: SetZero_sub
    private :: SetZeroRestart_sub

contains

    subroutine SetZero_sub ( me )

        class ( global ), target :: me

            me % grav    = zero
            me % DENSITY = zero
            me % DLT     = zero
            me % DLTMIN  = zero
            me % DLTTVD  = zero
            me % BETABR  = zero
            me % START   = zero
            me % HMAX2   = zero
            me % CURRENT = zero

            me % IMX    = 0
            me % KMX    = 0
            me % NBR    = 0
            me % NTR    = 0
            me % NWD    = 0
            me % NWB    = 0
            me % NCT    = 0
            me % NBOD   = 0
            me % NST    = 0
            me % NSP    = 0
            me % NGT    = 0
            me % NPI    = 0
            me % NPU    = 0
            me % NWDO   = 0
            me % NIKTSR = 0
            me % NUNIT  = 0
            me % JW     = 0
            me % JB     = 0
            me % JC     = 0
            me % IU     = 0
            me % ID     = 0
            me % KT     = 0
            me % I      = 0
            me % JJB    = 0
            me % NOD    = 0
            me % NDC    = 0
            me % NAL    = 0
            me % NSS    = 0
            me % NHY    = 0
            me % NFL    = 0
            me % NEP    = 0
            me % NEPT   = 0
            me % NZP    = 0
            me % NZPT   = 0
            me % JZ     = 0
            me % NZOOS  = 0
            me % NZOOE  = 0
            me % NMC    = 0
            me % NMCT   = 0

            me % CCTIME = ''
            me % CDATE  = ''
            me % RSIFN  = ''
            me % MODDIR = ''

    end subroutine SetZero_sub

    subroutine SetZeroRestart_sub ( me )

        class ( global ), target :: me

            me % grav    = zero
            me % DENSITY = zero
            me % DLT     = zero
            me % DLTMIN  = zero
            me % DLTTVD  = zero
            me % BETABR  = zero
            me % START   = zero
            me % HMAX2   = zero
            me % CURRENT = zero

    end subroutine SetZeroRestart_sub

end module mGlobals
