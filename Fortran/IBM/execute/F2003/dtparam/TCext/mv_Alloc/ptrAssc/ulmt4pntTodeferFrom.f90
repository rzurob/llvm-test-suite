! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/F2003/mv_Alloc/ptrAssc/ulmt4pntTodeferFrom.f
! opt variations: -qnok -qnol

! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : ulmt4pntTodeferFrom.f 
!*
!*  PROGRAMMER                 : Michelle Zhang 
!*  DATE                       : 06/13/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*                              
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : 1.FROM is of type character(:)
!*                               2.TO is function name of type unlimit poly 
!*                               3.A pointer component of type unlimited poly
!*                                   is associated with FROM
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

      character(:), allocatable, target :: ch2*12

      type A(k1,n1)    ! (4,20)
          integer, kind :: k1
          integer, len  :: n1
          class(*), pointer :: p
      end type


      contains
      function func(arg)
            class(*), allocatable :: func
            class(A(4,*)) :: arg
            target func

            call move_alloc(ch2, func)

            if  ( allocated(ch2) ) stop 11 
	    if ( .not. allocated(func) ) stop 13
 
            if ( .not. associated(arg%p, func) ) stop 21
      end function

end module


            use m

            type(A(4,20)) :: aA

            allocate(ch2, source= 'helloworld IBM' )

            aA%p => ch2

            select type ( x=> func(aA))
                type is (character(*))
                    select type ( y => aA%p)
                        type is (character(*))
                            if ( y /= 'helloworld I' ) stop 21 
                        class default
                            stop 31
                    end select
                class default
                    stop 33
            end select

      end
