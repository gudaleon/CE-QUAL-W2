! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
module mAllocator

    use mConstants,                     only : stdout, fmt_generic, error_fatal
    use mSetPrecision,                  only : ip

    implicit none
    integer                 :: alloc_status  = -1
    character ( len = 512 ) :: alloc_message = 'null'

    contains
        ! housekeeping_sub
        ! allocator_sub
        ! allocate_rank_2_rp_sub
        ! allocate_rank_1_rp_sub

    !  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =

    module subroutine allocate_rank_1_char_sub ( array, length )

        character ( len = * ), allocatable, intent ( inout ) :: array ( : )
        integer ( ip ),                     intent ( in )    :: length

            ! deallocate if needed
            if ( allocated ( array ) ) then
                write ( stdout, fmt_generic ) 'Warning: deallocating rank 1 array...'
                deallocate ( array, stat = alloc_status, errmsg = alloc_message )
                if ( alloc_status /= 0 ) then
                    write ( stdout, fmt_generic ) 'Error deallocating rank 1 array of ', length, ' elements, type character'
                    write ( stdout, fmt_generic ) 'error message: ', trim ( alloc_message ), '.'
                    write ( stdout, fmt_generic ) 'error number:  ', alloc_status
                    stop error_fatal
                end if
            end if

            ! allocate array
            allocate ( array ( 1 : length ), stat = alloc_status, errmsg = alloc_message )
            if ( alloc_status /= 0 ) then
                write ( stdout, fmt_generic ) 'Error allocating rank 1 array of ', length, ' elements, type character'
                write ( stdout, fmt_generic ) 'error message: ', trim ( alloc_message ), '.'
                write ( stdout, fmt_generic ) 'error number:  ', alloc_status
                stop error_fatal
            end if

            array ( : ) = '' ! populate and claim memory

    end subroutine allocate_rank_1_char_sub

end module mAllocator
