!******************************************************************************
!*  ===========================================================================
!*  XL Fortran Test Case                                  IBM INTERNAL USE ONLY
!*  ===========================================================================
!*
!*  TEST CASE NAME             : acetnone08a
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2006-08-03
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : BOZ+Hollerith literals in AC's with no type specifier (real)
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : typeless, boz, integer, real
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*
!*  DESCRIPTION
!*
!*  Boz literals and Hollerith constants are typeless, so including them in an
!*  array constructor lacking a type specifier is a problem, because we don't
!*  know what type to assign to the AC.  Here we test a mix of boz literals and
!*  Hollerith constants used in a real context.
!*
!*  Note: Unlike Hollerith constants, boz literals are part of the standard.
!*
!*  There are companion tests to these in types/intrinsics.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acetnone08a

  implicit none
  integer :: i
  integer(4)   :: intArray(2)
  real(4)      :: rlArray(2)

  rlArray = (/b'01100001011000010110000101100001', 4haaaa/);		print *, ' 1:', rlArray
  rlArray = (/o'14230461142', 4hbbbb/);					print *, ' 2:', rlArray
  rlArray = (/z'63636363',  4hcccc/);					print *, ' 3:', rlArray
  rlArray = (/(b'01100001011000010110000101100001',4haaaa, i=1,1) /);	print *, ' 4:', rlArray
  rlArray = (/(o'14230461142', 4hbbbb, i=1,1) /);			print *, ' 5:', rlArray
  rlArray = (/(z'63636363',4hcccc, i=1,1) /);				print *, ' 6:', rlArray

  rlArray = (/4haaaa, b'01100001011000010110000101100001'/);		print *, ' 7:', rlArray
  rlArray = (/4hbbbb, o'14230461142'/);					print *, ' 8:', rlArray
  rlArray = (/4hcccc, z'63636363'/);					print *, ' 9:', rlArray
  rlArray = (/(4haaaa, b'01100001011000010110000101100001', i=1,1) /);	print *, '10:', rlArray
  rlArray = (/(4hbbbb, o'14230461142', i=1,1) /);			print *, '11:', rlArray
  rlArray = (/(4hcccc, z'63636363', i=1,1) /);				print *, '12:', rlArray

  intArray = (/b'01100001011000010110000101100001', 4haaaa/);		print *, '13:', intArray
  intArray = (/o'14230461142', 4hbbbb/);				print *, '14:', intArray
  intArray = (/z'63636363',  4hcccc/);					print *, '15:', intArray
  intArray = (/(b'01100001011000010110000101100001',4haaaa, i=1,1) /);	print *, '16:', intArray
  intArray = (/(o'14230461142', 4hbbbb, i=1,1) /);			print *, '17:', intArray
  intArray = (/(z'63636363',4hcccc, i=1,1) /);				print *, '18:', intArray

  intArray = (/4haaaa, b'01100001011000010110000101100001'/);		print *, '19:', intArray
  intArray = (/4hbbbb, o'14230461142'/);				print *, '20:', intArray
  intArray = (/4hcccc, z'63636363'/);					print *, '21:', intArray
  intArray = (/(4haaaa, b'01100001011000010110000101100001', i=1,1) /);	print *, '22:', intArray
  intArray = (/(4hbbbb, o'14230461142', i=1,1) /);			print *, '23:', intArray
  intArray = (/(4hcccc, z'63636363', i=1,1) /);				print *, '24:', intArray

  call rtest(25,(/b'01100001011000010110000101100001', 4haaaa/))
  call rtest(26,(/o'14230461142', 4hbbbb/))
  call rtest(27,(/z'63636363',  4hcccc/))
  call rtest(28,(/(b'01100001011000010110000101100001',4haaaa, i=1,1) /))
  call rtest(29,(/(o'14230461142', 4hbbbb, i=1,1) /))
  call rtest(30,(/(z'63636363',4hcccc, i=1,1) /))

  call rtest(31,(/4haaaa, b'01100001011000010110000101100001'/))
  call rtest(32,(/4hbbbb, o'14230461142'/))
  call rtest(33,(/4hcccc, z'63636363'/))
  call rtest(34,(/(4haaaa, b'01100001011000010110000101100001', i=1,1) /))
  call rtest(35,(/(4hbbbb, o'14230461142', i=1,1) /))
  call rtest(36,(/(4hcccc, z'63636363', i=1,1) /))

  call itest(37,(/b'01100001011000010110000101100001', 4haaaa/))
  call itest(38,(/o'14230461142', 4hbbbb/))
  call itest(39,(/z'63636363',  4hcccc/))
  call itest(40,(/(b'01100001011000010110000101100001',4haaaa, i=1,1) /))
  call itest(41,(/(o'14230461142', 4hbbbb, i=1,1) /))
  call itest(42,(/(z'63636363',4hcccc, i=1,1) /))

  call itest(43,(/4haaaa, b'01100001011000010110000101100001'/))
  call itest(44,(/4hbbbb, o'14230461142'/))
  call itest(45,(/4hcccc, z'63636363'/))
  call itest(46,(/(4haaaa, b'01100001011000010110000101100001', i=1,1) /))
  call itest(47,(/(4hbbbb, o'14230461142', i=1,1) /))
  call itest(48,(/(4hcccc, z'63636363', i=1,1) /))

contains

  subroutine rtest(line,arg)
    integer :: line
    real(4) :: arg(:)
    print *, line, ':', size(arg), arg
  end subroutine rtest

  subroutine itest(line,arg)
    integer :: line
    integer(4) :: arg(:)
    print *, line, ':', size(arg), arg
  end subroutine itest

end program acetnone08a
