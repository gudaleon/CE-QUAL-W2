! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32

!***********************************************************************************************************************************
!**                                                                                                                               **
!**                                                         CE-QUAL-W2-PRE                                                        **
!**                                                          Version 4.0                                                          **
!**                                                                                                                               **
!**                                                     A preprocessor code for                                                   **
!**                                                           CE-QUAL-W2                                                          **
!**                                                                                                                               **
!**                                                   Developed by: Thomas M. Cole, Retired                                       **
!**                                                   Water Quality Modeling Group                                                **
!**                                                   U.S. Army Corps of Engineers                                                **
!**                                                   Waterways Experiment Station                                                **
!**                                                   Vicksburg, Mississippi 39180                                                **
!**                                                                                                                               **
!**                                                              and                                                              **
!**                                                                                                                               **
!**                                                          Scott A. Wells                                                       **
!**                                          Department of Civil and Environmental Engineering                                    **
!**                                                    Portland State University                                                  **
!**                                                         P.O. Box 751                                                          **
!**                                                  Portland, Oregon  97207-0751                                                 **
!**                                                  Phone number: (503) 725-4276                                                 **
!**                                                  FAX   number: (503) 725-5950                                                 **
!**                                                    e-mail wellss@pdx.edu                                                      **
!**                                                                                                                               **
!***********************************************************************************************************************************

module mMaster

    use mGridC,                         only : gridc
    use mIOHandles,                     only : io_handles
    use mReadInput,                     only : read_con_sub

    implicit none

    type ( io_handles ) :: myIO
    type ( gridc )      :: myGrid

contains

    subroutine develop ( )

        call myIO % open_files ( 'w2_con.npt', 'pre.err', 'pre.wrn', 'pre.opt', '../test/' )
        call read_con_sub ( myGrid, myIO )

    end subroutine develop

end module mMaster
