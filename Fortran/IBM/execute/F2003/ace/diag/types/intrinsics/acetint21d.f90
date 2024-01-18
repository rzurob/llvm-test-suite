!******************************************************************************
!*  ===========================================================================
!*
!*  TEST CASE NAME             : acetint21d
!*
!*  DATE                       : 2006-06-08
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : character type specifier variations
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : intrinsic type
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Verify that some character type specifiers are not allowed; there are 9 different
!*  valid forms, including 2 obsolete forms, although 27 variations might be conceived of.
!*  We look at the correctness of the value and that of the dynamic type and
!*  kind of the constructed array in a separate test.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acetint21d

  implicit none
  integer   :: un = 1
  character :: charr(3)

  ! All of these are wrong:

  ! Problems with length:
  charr = (/character(*):: 'aa','bb','cc'/)
  charr = (/character(:):: 'aa','bb','cc'/)
  charr = (/character(len=*):: 'aa','bb','cc'/)
  charr = (/character(len=:):: 'aa','bb','cc'/)

  charr = (/character(*,1):: 'aa','bb','cc'/)
  charr = (/character(:,1):: 'aa','bb','cc'/)
  charr = (/character(len=*,1):: 'aa','bb','cc'/)
  charr = (/character(len=:,1):: 'aa','bb','cc'/)

  charr = (/character(*,kind=1):: 'aa','bb','cc'/)
  charr = (/character(:,kind=1):: 'aa','bb','cc'/)
  charr = (/character(len=*,kind=1):: 'aa','bb','cc'/)
  charr = (/character(len=:,kind=1):: 'aa','bb','cc'/)

  charr = (/character(kind=1,len=*):: 'aa','bb','cc'/)
  charr = (/character(kind=1,len=:):: 'aa','bb','cc'/)

  ! Problems with kind:
  charr = (/character(1,2):: 'a', 'b', 'c' /)
  charr = (/character(1,kind=2):: 'a', 'b', 'c' /)
  charr = (/character(kind=2):: 'a', 'b', 'c' /)
  charr = (/character(kind=2,len=1):: 'a', 'b', 'c' /)

  charr = (/character(1,un):: 'a', 'b', 'c' /)
  charr = (/character(1,kind=un):: 'a', 'b', 'c' /)
  charr = (/character(kind=un):: 'a', 'b', 'c' /)
  charr = (/character(kind=un,len=1):: 'a', 'b', 'c' /)

  charr = (/character(1,one()):: 'a', 'b', 'c' /)
  charr = (/character(1,kind=one()):: 'a', 'b', 'c' /)
  charr = (/character(kind=one()):: 'a', 'b', 'c' /)
  charr = (/character(kind=one(),len=1):: 'a', 'b', 'c' /)

  ! Repeat with interposed asterisk:
  charr = (/character*(*):: 'aa','bb','cc'/)
  charr = (/character*(:):: 'aa','bb','cc'/)
  charr = (/character*(len=*):: 'aa','bb','cc'/)
  charr = (/character*(len=:):: 'aa','bb','cc'/)

  charr = (/character*(*,1):: 'aa','bb','cc'/)
  charr = (/character*(:,1):: 'aa','bb','cc'/)
  charr = (/character*(len=*,1):: 'aa','bb','cc'/)
  charr = (/character*(len=:,1):: 'aa','bb','cc'/)

  charr = (/character*(*,kind=1):: 'aa','bb','cc'/)
  charr = (/character*(:,kind=1):: 'aa','bb','cc'/)
  charr = (/character*(len=*,kind=1):: 'aa','bb','cc'/)
  charr = (/character*(len=:,kind=1):: 'aa','bb','cc'/)

  charr = (/character*(kind=1,len=*):: 'aa','bb','cc'/)
  charr = (/character*(kind=1,len=:):: 'aa','bb','cc'/)

  charr = (/character*(1,2):: 'a', 'b', 'c' /)
  charr = (/character*(1,kind=2):: 'a', 'b', 'c' /)
  charr = (/character*(kind=2):: 'a', 'b', 'c' /)
  charr = (/character*(kind=2,len=1):: 'a', 'b', 'c' /)

  charr = (/character*(1,un):: 'a', 'b', 'c' /)
  charr = (/character*(1,kind=un):: 'a', 'b', 'c' /)
  charr = (/character*(kind=un):: 'a', 'b', 'c' /)
  charr = (/character*(kind=un,len=1):: 'a', 'b', 'c' /)

  charr = (/character*(1,one()):: 'a', 'b', 'c' /)
  charr = (/character*(1,kind=one()):: 'a', 'b', 'c' /)
  charr = (/character*(kind=one()):: 'a', 'b', 'c' /)
  charr = (/character*(kind=one(),len=1):: 'a', 'b', 'c' /)


contains

  integer function one()
    one = 1
  end function one

end program acetint21d
