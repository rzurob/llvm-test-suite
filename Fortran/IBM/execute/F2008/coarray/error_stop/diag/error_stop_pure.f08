!*
!*  ===================================================================
!*
!*  TYPE                       : Duagnostic test
!*  FEATURE                    : #351605.31 CAF - ERROR STOP statement
!*
!*  DATE                       : 28 August 2010
!*
!*  REQUIRED COMPILER OPTIONS  :
!*  DEPENDENCIES               :
!*
!*  DESCRIPTION                : Test that a pure subprogram must not
!*                               contain an ERROR STOP statement
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

      ELEMENTAL PURE SUBROUTINE elemsub11(A1,A2)
       complex*16, INTENT(in)    :: A1
       complex*16, INTENT(inout) :: A2
       A2=A1*A2
       ERROR STOP "In ELEMENTAL PURE SUBROUTINE"
      END SUBROUTINE elemsub11

     module M
      contains
       pure function s1(a1)
           integer, intent(in):: a1
           integer s1
           s1 = 4
           error stop 4
       end function s1
       pure function s2(a1,a2)
           integer, intent(in):: a1,a2
           integer s2
           s2 = 5
           error stop 5
       end function s2
       pure function s3(a1,a2,a3)
           integer, intent(in):: a1,a2,a3
           integer s3
           s3 = 6
           error stop 6
       end function s3
       pure function s4(a1,a2,a3,a4)
           integer, intent(in):: a1,a2,a3,a4
           integer s4
           s4 = 7
           error stop 7
       end function s4
       pure function s5(a1,a2,a3,a4,a5)
           integer, intent(in):: a1,a2,a3,a4,a5
           integer s5
           s5 = 8
           error stop 8
       end function s5

     end module

     pure integer function func1(r1)
          real, intent(in):: r1
          func1 = 9
          error stop 9
     end function func1
     pure integer function func2(r1,r2)
          real, intent(in):: r1,r2
          func2 = 10
          error stop 10
     end function func2

     subroutine sub()
     use M
     interface ss
         module procedure s1
         pure integer function func1(r1)
              real, intent(in):: r1
         end function func1
         module procedure s2,s3
         module procedure s4
         pure integer function func2(r1,r2)
              real, intent(in):: r1,r2
         end function func2
         module procedure s5
     end interface
     integer array1(ss(1))
     integer array2(ss(1,2))
     integer array3(ss(1,2,3))
     integer array4(ss(1,2,3,4))
     integer array5(ss(1,2,3,4,5))
     integer array6(ss(1.0))
     integer array7(ss(1.0,2.0))

     if (size(array1) /=  4) error stop 1
     if (size(array2) /=  5) error stop 2
     if (size(array3) /=  6) error stop 3
     if (size(array4) /=  7) error stop 4
     if (size(array5) /=  8) error stop 5
     if (size(array6) /=  9) error stop 6
     if (size(array7) /= 10) error stop 7
     end subroutine sub

     program test
     call sub()
     end
