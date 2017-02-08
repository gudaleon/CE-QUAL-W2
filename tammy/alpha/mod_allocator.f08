! ! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
! module mAllocator
!
!     integer                 :: alloc_status  = -1
!     character ( len = 512 ) :: alloc_message = 'null'
!
!     contains
!         ! housekeeping_sub
!         ! allocator_sub
!         ! allocate_rank_2_rp_sub
!         ! allocate_rank_1_rp_sub
!
!     !  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =
!
!     module subroutine housekeeping_sub ( me ) ! allocate, initialize, pointer to diagonal
!
!         class ( KalmanData ), target :: me
!
!             call allocator_sub ( me ) ! allocate all arrays
!
!     end subroutine housekeeping_sub
!
!     !  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =
!
!     module subroutine allocator_sub ( me )
!
!         class ( KalmanData ), target :: me
!
!             ! rank 2
!             call allocate_rank_2_rp_sub ( me % pcm_p, me % numDataPoints, me % numDataPoints )
!
!             ! pointer to diagonal elements
!             ! Metcalf, Cohen, Reid, figure 20.5, p. 364
!             pcmp_flattened ( 1 : me % numDataPoints * me % numDataPoints ) => me % pcm_p ( : , : )
!             pcmp_diagonal  => pcmp_flattened ( :: me % numDataPoints + 1 )
!
!             call allocate_rank_2_rp_sub ( me % tmat,  me % numDataPoints, me % numDataPoints )
!             ! rank 1
!             call allocate_rank_1_rp_sub ( me % dv_x, me % numDataPoints )
!             call allocate_rank_1_rp_sub ( me % gv_k, me % numDataPoints )
!             call allocate_rank_1_rp_sub ( me % fv_f, me % numDataPoints )
!
!     end subroutine allocator_sub
!
!     !  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =
!
!     module subroutine allocate_rank_2_rp_sub ( array, length1, length2 )
!
!         real ( rp ), allocatable, intent ( inout ) :: array ( : , : )
!         integer ( ip ),           intent ( in )    :: length1, length2
!
!             ! deallocate if needed
!             if ( allocated ( array ) ) then
!                 write ( stdout, fmt_generic ) 'Warning: deallocating rank 1 array...'
!                 deallocate ( array, stat = alloc_status, errmsg = alloc_message )
!                 if ( alloc_status /= 0 ) then
!                 write ( stdout, fmt_generic ) 'Error deallocating rank 2 array of ', length1, ' x ', &
!                                                                                      length2, ' elements, type real ( rp ).'
!                 write ( stdout, fmt_generic ) 'Error message: ', trim ( alloc_message ), '.'
!                 write ( stdout, fmt_generic ) 'Error number:  ', alloc_status, '.'
!                 stop error_fatal
!                 end if
!             end if
!
!             ! allocate array
!             allocate ( array ( 1 : length1, 1 : length2 ), stat = alloc_status, errmsg = alloc_message )
!             if ( alloc_status /= 0 ) then
!                 write ( stdout, fmt_generic ) 'Error deallocating rank 2 array of ', length1, ' x ', &
!                                                                                      length2, ' elements, type real ( rp ).'
!                 write ( stdout, fmt_generic ) 'Error message: ', trim ( alloc_message ), '.'
!                 write ( stdout, fmt_generic ) 'Error number:  ', alloc_status, '.'
!                 stop error_fatal
!             end if
!
!             array ( : , : ) = zero  ! populate and claim memory
!
!     end subroutine allocate_rank_2_rp_sub
!
!     !  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =
!
!     module subroutine allocate_rank_1_rp_sub ( array, length )
!
!         real ( rp ), allocatable, intent ( inout ) :: array ( : )
!         integer ( ip ),           intent ( in )    :: length
!
!             ! deallocate if needed
!             if ( allocated ( array ) ) then
!                 write ( stdout, fmt_generic ) 'Warning: deallocating rank 1 array...'
!                 deallocate ( array, stat = alloc_status, errmsg = alloc_message )
!                 if ( alloc_status /= 0 ) then
!                     write ( stdout, fmt_generic ) 'Error deallocating rank 1 array of ', length, ' elements, type real ( rp )'
!                     write ( stdout, fmt_generic ) 'error message: ', trim ( alloc_message ), '.'
!                     write ( stdout, fmt_generic ) 'error number:  ', alloc_status
!                     stop error_fatal
!                 end if
!             end if
!
!             ! allocate array
!             allocate ( array ( 1 : length ), stat = alloc_status, errmsg = alloc_message )
!             if ( alloc_status /= 0 ) then
!                 write ( stdout, fmt_generic ) 'Error allocating rank 1 array of ', length, ' elements, type integer ( ip )'
!                 write ( stdout, fmt_generic ) 'error message: ', trim ( alloc_message ), '.'
!                 write ( stdout, fmt_generic ) 'error number:  ', alloc_status
!                 stop error_fatal
!             end if
!
!             array ( : ) = zero ! populate and claim memory
!
!     end subroutine allocate_rank_1_rp_sub
!
! end module mAllocator
