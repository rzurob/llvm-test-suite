!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Aug 31, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION
!* - pointer used as arg of matmul
!* - type logical, bounds-remapping-list
!* - matmul( A of shape(3,2), B of shape(2))
!* - matmul( A of shape(3), B of shape(3,2))
!* - pointer's element is used as AC value
!*
!234567890123456789012345678901234567890123456789012345678901234567890

	program main

	logical, pointer :: p(:,:)
	logical, target, allocatable :: t(:)

	allocate(t(20), source=(/( mod(i,2) == 1, i=1,20 ) /))

	p(1:3,1:2) => t(11:16)

	if ( .not. associated(p) ) error stop 5

	do i = 1, 3
	    do j = 1, 2
		p(i:, j:) => p
	    enddo
 	enddo

	if ( .not. associated(p) ) error stop 6
	if ( any (lbound(p) .ne. (/3,2/))  ) error stop 7
	if ( any (ubound(p) .ne. (/5,3/))  ) error stop 9

        if ( any( matmul((/p(3,3), p(4,3), p(5,2)/), p) .neqv. &
                  (/ .true.,.true./) )) error stop 11

        if (any (matmul(p, (/.false., .true./)) .neqv. &
                  (/ .false., .true., .false./) )) error stop 21

	End program