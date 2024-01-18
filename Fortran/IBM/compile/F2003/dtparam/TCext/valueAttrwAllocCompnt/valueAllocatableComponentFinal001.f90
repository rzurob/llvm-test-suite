! GB DTP extension using:
! ftcx_dtp -qk -qnol -qnodefaultpv -qreuse=self /tstdev/F2003/valueAttrwAllocCompnt/valueAllocatableComponentFinal001.f
! opt variations: -qnok -ql -qdefaultpv -qreuse=none

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Value Attribute for derived type containing allocatable components
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : value attribute with derived type containing allocatable components
!*                                 - finalization procedure containing value attribute dummy arg
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

   type inner(k1)    ! (4)
      integer, kind        :: k1
      integer(k1), pointer :: i
   end type

   type base(k2)    ! (4)
      integer, kind                :: k2
      type(inner(k2)), allocatable :: inb
      contains
         final :: finalbase
   end type

   integer, target :: i

   contains

      subroutine finalbase ( a )
         type(base(4)), value :: a

         print *, 'finalbase'
         if ( associated (a%inb%i) ) then
            print *, associated ( a%inb%i,i )
            print *, a%inb%i
            a%inb%i = -999

         end if
         print *, 'done', allocated(a%inb)

      end subroutine

end module

program valueAllocatableComponentFinal001
   use m

   type(base(4)) :: b1
   type(base(4)), allocatable :: b2
   i = 10

   print *, 'finalize b1, and base():'
   b1 = base(4)(inner(4)(i))
   print *, i

   i = 100

   print *, 'finalize base():'
   allocate ( b2, source = base(4)(inner(4)(i)) )
   print *,i

   i = 1000
   print *, 'finalize b2:'
   deallocate ( b2 )
   print *,i

end program
