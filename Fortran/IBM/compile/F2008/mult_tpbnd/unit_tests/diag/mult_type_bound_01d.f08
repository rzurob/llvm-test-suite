! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2011-05-20
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : within module
!*                               interface shall not have " => binding "
!*                               procedure(iface), deferred, pass :: foo => real_foo,bar => real_bar
!*                               1st and 2nd both incorrect

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
         procedure(iface), deferred, pass :: foo => real_foo,bar => real_bar !1st and 2nd both incorrect
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
         procedure, pass :: foo=> real_foo, bar => real_bar,fox => real_fox
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

      subroutine real_fox(dtv)
         class(fox_child(4,4)), intent(inout) :: dtv
         print *,"real_fox:", dtv%i * dtv%j
      end subroutine

end module
