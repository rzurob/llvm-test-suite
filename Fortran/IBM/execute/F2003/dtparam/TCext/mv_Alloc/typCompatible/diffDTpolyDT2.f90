! GB DTP extension using:
! ftcx_dtp -qk -qdeferredlp /tstdev/F2003/mv_Alloc/typCompatible/diffDTpolyDT2.f
! opt variations: -qck -qnok -qnodeferredlp

! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : diffDTpolyDT2.f 
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
!*  DESCRIPTION                : FROM is of an nonpoly child DT 
!*                               TO is of an poly parent DT, dummy arg
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


      type  :: base(k1,n1)    ! (4,10)
          integer, kind :: k1
          integer, len  :: n1
          character(n1)    ch 
      end type 

      type, extends(base) :: child    ! (4,10)
      end type 

      class(base(4,:)), allocatable :: b
      type(child(4,:)), allocatable :: a 

      allocate(b, source=( base(4,10)('helloworld') ) )
      allocate(a, source=( child(4,10)('COMPILER') ) )

      call sub(b)
     
      if ( allocated(a) ) stop 11
      if ( .not. allocated(b) ) stop 12
 
      select type ( b )
          type is (child(4,*))
             if ( b%ch /= 'COMPILER')  STOP 21
          class default
             STOP 23
      end select
             
      contains 
         subroutine sub(brg)
            class(base(4,:)), optional :: brg 
            allocatable  brg 

            call move_alloc(a, brg) 
            
         end subroutine
      end
