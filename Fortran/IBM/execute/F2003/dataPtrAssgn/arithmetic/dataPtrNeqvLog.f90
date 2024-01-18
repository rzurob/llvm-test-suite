!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrNeqvLog.f
!*
!*  PROGRAMMER                 : Michelle Zhang
!*  DATE                       : Aug 31, 2006
!*  ORIGIN                     : Compiler Development, IBM Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION
!* - data-ptr has bounds-remapping list; data-ptr of type class(*) and data-tar
!*   of type logical*8 are components of two different DTs where one DT with 
!*   data-ptr as component is grand-child of the other DT with logical component!* 
!234567890123456789012345678901234567890123456789012345678901234567890

module m

    type A
	class(*), pointer :: p(:,:,:)
    end type

    type, extends(A) :: B
    end type

    type, extends(B) :: C
    end type

    type, extends(C) :: D
    end type

    type, extends(D) :: E
	logical*8, allocatable :: tar(:)
    end type 

end module

program main

    use m
    
    type(E), target :: e1

    allocate(e1%tar(4), source = logical((/ .true.,.false., .true., .false. /),8) )
    e1%p(3:3,0:0,2:5) => e1%tar(4:1:-1)

    if ( .not. associated(e1%p)) stop 1
    if ( any (lbound(e1%p) .ne. (/3,0,2/))) stop 2 
    if ( any (ubound(e1%p) .ne. (/3,0,5/))) stop 3 
 
    select type(x=>e1%p) 
	type is (logical*8)
	    print *, x
	    if ( x(3,0,2) .neqv. .false. ) stop 4
	    if ( x(3,0,3) .neqv. .true. ) stop 5 
	    if ( x(3,0,4) .neqv. .false. ) stop 6 
	    if ( x(3,0,5) .neqv. .true. ) stop 7 
  	class default
            stop 9
    end select

end program

