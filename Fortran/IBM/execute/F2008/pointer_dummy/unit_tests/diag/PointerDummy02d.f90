!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : June 22, 2011
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 916820
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION:
!*  -----------
!*  Fortran 2008 allows procedure calls where:
!*        1. The dummy argument has the POINTER and INTENT(IN) attributes, and
!*        2. The actual argument is a nonpointer that has the TARGET attribute
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
  contains
    subroutine mod_proc(c)
      character(*) :: c

      print *, c
  end subroutine
end module

program PointerDummyDiag2
  use m

  interface
    subroutine print_hello()
    end subroutine

    subroutine ext_proc(c)
      character(*) :: c
    end
  end interface

  character(20), target :: c1, c2, c3
  procedure(), pointer :: p1, p2, p3

  c1 = "External Procedure"
  c2 = "Internal Procedure"
  c3 = "Module Procedure"

  p1 => ext_proc
  p2 => inter_proc
  p3 => mod_proc

  call ext_test(ext_proc)
  call inter_test(inter_proc)
  call mod_test(mod_proc)

  call ext_test(p1)
  call inter_test(p2)
  call mod_test(p3)

  contains
    subroutine ext_test(dmy_pp)
    procedure(), pointer, intent(in) :: dmy_pp

    call dmy_pp("ext_test")
  end

  subroutine inter_test(dmy_pp)
    procedure(), pointer, intent(in) :: dmy_pp

    call dmy_pp("inter_test")
  end

  subroutine mod_test(dmy_pp)
    procedure(), pointer, intent(in) :: dmy_pp

    call dmy_pp("mod_test")
  end

  subroutine inter_proc(c)
    character(*) :: c

    print *, c
  end
end

subroutine ext_proc(c)
  character(*) :: c

  print *, c
end