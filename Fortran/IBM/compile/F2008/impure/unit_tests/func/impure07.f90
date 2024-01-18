!*  ============================================================================
!*
!*  TEST CASE NAME             : impure07f.f
!*
!*  DATE                       : 2012-03-08
!*  ORIGIN                     : Compiler Development, IBM China Development Shanghai Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : impure procedures
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 917300
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : Fortran 2008 support for the IMPURE attribute
!*                               for procedures,which allows for ELEMENTAL procedures
!*                               without the restrictions of PURE.
!*                               C1285/6 is not apply to IMPURE procedures
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789
      impure elemental subroutine sub(para)
        implicit none
        logical,intent(in):: para
        character(len=20):: string
        logical :: logres
        integer ios
        print *,"C1285 allowed here"
        write(*,*) "C1285 also allowed here"
        read(*,*)
        inquire(file="hello.txt",exist=logres)
        open(unit=10,file="hello.txt")
        write(10,*) "hello"
        flush(10)
        backspace(10,iostat=ios)
        read(10,*) string
        endfile(10,iostat=ios)
        rewind(10,iostat=ios)
        wait(10)  ! a bug here for pure procedures?
        close(10)
      end
