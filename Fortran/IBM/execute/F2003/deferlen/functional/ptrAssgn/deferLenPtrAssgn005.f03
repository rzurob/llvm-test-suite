!*  ===================================================================
!*
!*  DATE                       : 05/01/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Characters with deferred length type parameter
!*
!*  DESCRIPTION                : scalar/array character with deferred length with
!*                               pointer assignment that points to zero length characters
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module n
   character(:), pointer :: c1
end module

program deferLenPtrAssgn005
   use n

   character(kind=1,len=:), pointer :: c2(:)
   character(0), target :: abc, abcs(10)

   c1 => abc

   print *, len(c1), c1

   c2 => abcs
   print *, len(c2), size(c2), c2

   allocate ( c1, source = 'abcdefghi' )

   c1 => c2(10)
   print *, len(c1), c1

   allocate ( c2(0), source = (/ character(0) :: /) )

   print *, len(c2), size(c2), c2

end program
