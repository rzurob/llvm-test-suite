! GB DTP extension using:
! ftcx_dtp -qck -ql /tstdev/F2003/generic/syntax/C459/genericC459GenericName002d.f
! opt variations: -qnock -qnol

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with generic name
!*
!*  DESCRIPTION                : C459: define generic TB with same generic name with different access-spec
!*                                     within same derived type ( inheritance )
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

   type base(n1,k1)    ! (20,4)
      integer, kind :: k1
      integer, len  :: n1
      integer(k1)   :: id
      contains
         private
         procedure, pass :: setid
         generic :: set => setid
   end type

   type, extends(base) :: child(k2,n2)    ! (20,4,1,3)
      integer, kind             :: k2
      integer, len              :: n2
      character(kind=k2,len=n2) :: c
      contains
         procedure, pass :: setname
         generic :: set => setname
   end type

   type, extends(base) :: child1(k3,n3)    ! (20,4,1,3)
      integer, kind             :: k3
      integer, len              :: n3
      character(kind=k3,len=n3) :: c
      contains
         procedure, nopass :: setname
         generic :: set => setname
   end type

   contains

      subroutine setid ( a , i )
         class(base(*,4)), intent(inout) :: a
         integer, intent(in) :: i

      end subroutine

      subroutine setname ( a , c )
         class(child(*,4,1,*)), intent(inout) :: a
         character(3), intent(in) :: c

      end subroutine

end module

program genericC459Assignment002d
end program
