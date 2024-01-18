! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/mv_Alloc/typCompatible/implictype.f
! opt variations: -ql

! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : implictype.f
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
!*  DESCRIPTION                : FROM/TO are of a derived-type 
!*                               use implict to specify default type
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


     program main 

          type t(k1)    ! (4)
             integer, kind :: k1
             sequence
             integer(k1)      i
          end type

          implicit type(t(4))  (a)

          allocatable a1

          call sub()

          contains
               subroutine sub
                  implicit type(t(4))  (n)
               
                  allocatable n1 

                  allocate(a1, source = t(4)(10))
                  call move_alloc(a1, n1)
                  if ( .not. allocated(n1) ) stop 21 
                  if ( allocated(a1) ) stop 31
                  if ( n1%i /= 10 ) stop 41
               end subroutine

     end program

