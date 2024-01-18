! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : mult_type_deferred.f 
!*
!*  PROGRAMMER                 : Paul Liu
!*  DATE                       : 2011-05-20
!*  ORIGIN                     :
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              :
!*
!*  DESCRIPTION                : within module
!*                               deferred attribute
!*                               in base type: procedure(iface), deferred, pass :: foo,bar
!*                               in extend type: procedure, pass :: foo=> real_foo, bar => real_bar

!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

   type, abstract :: fox_base(k1)    ! (4)
      integer, kind :: k1
      integer(k1)   :: i
      contains
         procedure(iface), deferred, pass :: foo,bar
   end type

   interface
      subroutine iface(dtv)
         import fox_base
         class(fox_base(4)), intent(inout) :: dtv
      end subroutine
   end interface

   type, extends(fox_base) :: fox_child(k2)    ! (4,4)
      integer, kind :: k2
      integer(k2)   :: j
      contains
         procedure, pass :: foo=> real_foo, bar => real_bar
   end type

   contains

      subroutine real_foo(dtv)
         class(fox_child(4,4)), intent(inout) :: dtv 
         print *,"real_foo:", dtv%i - dtv%j
      end subroutine

      subroutine real_bar(dtv)
         class(fox_child(4,4)), intent(inout) :: dtv 
         print *,"real_bar:", dtv%i + dtv%j
      end subroutine

end module


program t
   use m

   type(fox_child(4,4)) fox_c
   fox_c%i = 2
   fox_c%j = 3
   call fox_c%foo()
   call fox_c%bar()

end program
