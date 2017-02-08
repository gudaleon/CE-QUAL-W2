! Tomas Mondragon
! Computer Scientist
! USACE ERDC Information Technology Laboratory
! Computational Analysis Branch
! Tomas.A.Mondragon@erdc.dren.mil
!   delivered 06 Feb 2016
!   updated   21 Jan 2017

!  functions
!   safeopen_readonly(filename)
!   safeopen_readwrite(filename)
!   safeopen_writenew(filename)
!   safeopen_writeappend(filename)
!   safeopen_writereplace(filename)
!   safeopen_scratchfile()
!   close_and_keep_scratchfile(fd)
!  subroutines
!   close_and_delete(fd)
!   find_IU_info(fd)
MODULE mFileHandling

    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY : INPUT_UNIT, OUTPUT_UNIT, ERROR_UNIT

    IMPLICIT NONE

    INTEGER, PARAMETER  :: stdout = OUTPUT_UNIT
    INTEGER, PARAMETER  :: stdin  = INPUT_UNIT
    INTEGER, PARAMETER  :: stderr = ERROR_UNIT

CONTAINS

    ! Open a file in read only mode
    ! USAGE: fd = safeopen_readonly(filename)
    FUNCTION safeopen_readonly(filename) RESULT(fd)
        IMPLICIT NONE
        INTEGER                                       :: fd
        CHARACTER(LEN=*),INTENT(IN)                   :: filename
        CHARACTER(LEN=256)                            :: io_message = ""
        INTEGER                                       :: io_err
        LOGICAL                                       :: opened, exists

            INQUIRE(FILE=TRIM(filename), EXIST=exists, OPENED=opened)
            IF(.NOT. exists) THEN
                WRITE(stderr,'(A,A,A)') "ERROR: Could not open ",TRIM(filename)," for reading; file doesn't exist."
                STOP
            ELSE IF (opened) THEN
                WRITE(stderr,'(A,A,A)') "ERROR: Refused to open ",TRIM(filename)," for reading; file already opened."
                STOP
            END IF

            OPEN(NEWUNIT=fd, FILE=TRIM(filename), ACTION='READ', IOSTAT=io_err, IOMSG=io_message)
            IF(io_err .NE. 0) THEN
                WRITE(stderr,'(A)') io_message
                STOP
            END IF
    END FUNCTION safeopen_readonly

    ! Open a file in readwrite mode
    ! USAGE: fd = safeopen_readwrite(filename)
    FUNCTION safeopen_readwrite(filename) RESULT(fd)
        IMPLICIT NONE
        INTEGER                                       :: fd
        CHARACTER(LEN=*),INTENT(IN)                   :: filename
        CHARACTER(LEN=256)                            :: io_message = ""
        INTEGER                                       :: io_err
        LOGICAL                                       :: opened, exists

        INQUIRE(FILE=TRIM(filename), EXIST=exists, OPENED=opened)
        IF(.NOT. exists) THEN
            WRITE(stderr,'(A,A,A)') "Warning: ",TRIM(filename)," doesn't exist; new empty file will be created."
        ELSE IF (opened) THEN
            WRITE(stderr,'(A,A,A)') "ERROR: Refused to open ",TRIM(filename),"; file already opened."
            STOP
        END IF

        OPEN(NEWUNIT=fd, FILE=TRIM(filename), ACTION='READWRITE', IOSTAT=io_err, IOMSG=io_message)
        IF(io_err .NE. 0) THEN
            WRITE(stderr,'(A)') io_message
            STOP
        END IF
    END FUNCTION safeopen_readwrite

    ! Open a new file for writing
    ! USAGE: fd = safeopen_writenew(filename)
    FUNCTION safeopen_writenew(filename) RESULT(fd)
        IMPLICIT NONE
        INTEGER                                       :: fd
        CHARACTER(LEN=*),INTENT(IN)                   :: filename
        CHARACTER(LEN=256)                            :: io_message = ""
        INTEGER                                       :: io_err
        LOGICAL                                       :: opened, exists

            INQUIRE(FILE=TRIM(filename), EXIST=exists, OPENED=opened)
            IF(exists) THEN
                WRITE(stderr,'(A,A,A)') "ERROR: ",TRIM(filename)," already exists; refused to open in case of overwrite."
                STOP
            ELSE IF (opened) THEN
                WRITE(stderr,'(A,A,A)') "ERROR: ",TRIM(filename)," doesn't exist but is already open;", &
                                        "something is seriously wrong with your OS."
                STOP
            END IF

            OPEN(NEWUNIT=fd, FILE=TRIM(filename), ACTION='WRITE', STATUS='NEW', IOSTAT=io_err, IOMSG=io_message)
            IF(io_err .NE. 0) THEN
                WRITE(stderr,'(A)') io_message
                STOP
            END IF
    END FUNCTION safeopen_writenew

    ! Open a file for writing, appending new content to the file's previous content
    ! USAGE: fd = safeopen_writeappend(filename)
    FUNCTION safeopen_writeappend(filename) RESULT(fd)
        IMPLICIT NONE
        INTEGER                                       :: fd
        CHARACTER(LEN=*),INTENT(IN)                   :: filename
        CHARACTER(LEN=256)                            :: io_message = ""
        INTEGER                                       :: io_err
        LOGICAL                                       :: opened, exists

            INQUIRE(FILE=TRIM(filename), EXIST=exists, OPENED=opened)
            IF(.NOT.exists) THEN
                WRITE(stderr,'(A,A,A)') "Warning: ",TRIM(filename)," doesn't exist; new empty file will be created."
            ELSE IF (opened) THEN
                WRITE(stderr,'(A,A,A)') "ERROR: ",TRIM(filename)," is already open; refused to open to prevent ", &
                                        "unintentional writing."
                STOP
            END IF

            IF (exists) THEN
                OPEN(NEWUNIT=fd, FILE=TRIM(filename), STATUS="old", POSITION="append", ACTION="write", IOSTAT=io_err, &
                     IOMSG=io_message)
                IF(io_err .NE. 0) THEN
                    WRITE(stderr,'(A)') io_message
                    STOP
                END IF
            ELSE
                OPEN(NEWUNIT=fd, FILE=TRIM(filename), STATUS="new", ACTION="write", IOSTAT=io_err, IOMSG=io_message)
                IF(io_err .NE. 0) THEN
                    WRITE(stderr,'(A)') io_message
                STOP
                END IF
            END IF
    END FUNCTION safeopen_writeappend

    ! Open a file for writing, overwriting its previous content
    ! USAGE: fd = safeopen_writereplace(filename)
    FUNCTION safeopen_writereplace(filename) RESULT(fd)
        IMPLICIT NONE
        INTEGER                                       :: fd
        CHARACTER(LEN=*),INTENT(IN)                   :: filename
        CHARACTER(LEN=256)                            :: io_message = ""
        INTEGER                                       :: io_err
        LOGICAL                                       :: opened, exists

            INQUIRE(FILE=TRIM(filename), EXIST=exists, OPENED=opened)
            IF(.NOT.exists) THEN
                WRITE(stderr,'(A,A,A)') "Warning: ",TRIM(filename)," doesn't exist; new empty file will be created."
            ELSE IF (opened) THEN
                WRITE(stderr,'(A,A,A)') "ERROR: ",TRIM(filename)," is already open; refused to open to prevent ", &
                                        "unintentional writing."
                STOP
            END IF

            OPEN(NEWUNIT=fd, FILE=TRIM(filename), STATUS="REPLACE", ACTION="write", IOSTAT=io_err, IOMSG=io_message)
            IF(io_err .NE. 0) THEN
                WRITE(stderr,'(A)') io_message
                STOP
            END IF
    END FUNCTION safeopen_writereplace

    ! Open a scratch file, which will be deleted upon closing by default
    ! USAGE: fd = safeopen_scratchfile()
    FUNCTION safeopen_scratchfile() RESULT(fd)
        IMPLICIT NONE
        INTEGER                                       :: fd
        CHARACTER(LEN=256)                            :: io_message = ""
        INTEGER                                       :: io_err

            OPEN(NEWUNIT=fd, STATUS="SCRATCH", ACTION="READWRITE", IOSTAT=io_err, IOMSG=io_message)
            IF(io_err .NE. 0) THEN
                WRITE(stderr,'(A)') io_message
                STOP
            END IF
    END FUNCTION safeopen_scratchfile

    ! Special close for the special occasion when a file should be deleted after being closed
    ! don't bother using for files opened with the status SCRATCH; fortran deletes those on close already
    ! USAGE: CALL close_and_delete(fd)
    SUBROUTINE close_and_delete(fd)
        IMPLICIT NONE
        INTEGER,INTENT(IN)                            :: fd
        LOGICAL                                       :: opened

        INQUIRE(UNIT=fd, OPENED=opened)
        IF (.NOT.opened) THEN
            WRITE(stderr,'(A,i4,A)') "ERROR: file descriptor unit ",fd," is not open; refused to delete ", &
                                     "to prevent unintentional deletion."
            STOP
        END IF
        CLOSE(UNIT=fd,STATUS='DELETE')
    END SUBROUTINE close_and_delete

    ! Special close function for the rare occasion you want to keep a scratchfile
    ! USAGE: newfilename = close_and_keep_scratchfile(fd)
    FUNCTION close_and_keep_scratchfile(fd) RESULT(filename)
        IMPLICIT NONE
        INTEGER, INTENT(IN)                           :: fd
        CHARACTER(LEN=256)                              :: filename
        LOGICAL                                       :: named, opened

            INQUIRE(UNIT=fd, NAMED=named, OPENED=opened)
            IF (opened) THEN
                IF (named) THEN
                    INQUIRE(UNIT=fd,NAME=filename)
                    WRITE(stderr,'(A,i6,A,A,A)') "File descriptor unit ",fd," is named ",TRIM(filename),"."
                ELSE
                    WRITE(stderr,'(A,i6,A)') "File descriptor unit ",fd," is unnamed. Naughty computer!"
                    STOP
                END IF
                    CLOSE(UNIT=fd,STATUS="KEEP")
            ELSE
                WRITE(stderr,'(A,i6,A)') "File descriptor unit ",fd," isn't open. Naughty programmer!"
                STOP
            END IF
    END FUNCTION close_and_keep_scratchfile

    ! This is just an informative printer of information about a given IO file descriptor unit
    ! USAGE: CALL find_IU_info(fd)
    SUBROUTINE find_IU_info(fd)
        IMPLICIT NONE
        INTEGER,INTENT(IN)                            :: fd
        CHARACTER(LEN=256)                            :: filename, omode, pos
        LOGICAL                                       :: opened, exists, named

            INQUIRE(UNIT=fd, OPENED=opened, EXIST=exists, NAMED=named, ACTION=omode, POSITION=pos)
            IF (opened) THEN
                WRITE(stdout,'(A,i6,A,A,A,A,A)') "File descriptor unit ",fd," is opened &
                                                 &in mode ",TRIM(omode), " at position ",TRIM(pos),"."
            ELSE
                WRITE(stdout,'(A,i6,A)') "File descriptor unit ",fd," is not open."
            END IF
            IF (exists) THEN
                WRITE(stdout,'(A,i6,A)') "File descriptor unit ",fd," is in the range of values &
                                           &allowed by the compiler."
            ELSE
                WRITE(stdout,'(A,i6,A)') "File descriptor unit ",fd," not allowed by the compiler."
            END IF
            IF (named) THEN
                INQUIRE(UNIT=fd,NAME=filename)
                WRITE(stdout,'(A,i6,A,A,A)') "File descriptor unit ",fd," is named ",TRIM(filename),"."
            ELSE
                WRITE(stdout,'(A,i6,A)') "File descriptor unit ",fd," is unnamed."
            END IF
    END SUBROUTINE find_IU_info

END MODULE mFileHandling
