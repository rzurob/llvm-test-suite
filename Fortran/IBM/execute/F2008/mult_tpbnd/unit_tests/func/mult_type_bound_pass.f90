! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2011-05-20
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : within module
!*                               pass attribute
!*                               procedure,pass(a) :: foo=>real_foo,bar=>real_bar,fox=>real_fox

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

      type point(K)
        integer, kind :: K
        integer(K):: data_a
        contains
        procedure,pass(a) :: foo=>real_foo,bar=>real_bar,fox=>real_fox
      end type point

      contains

      integer function real_foo(a,b)
        class(point(4)) :: a,b
        real_foo = a%data_a + b%data_a
        print *,"real_foo = ",real_foo
      end

      integer function real_bar(a,b)
        class(point(4)) :: a,b
        real_bar = a%data_a - b%data_a
        print *,"real_bar = ",real_bar
      end

      integer function real_fox(a,b,c)
        class(point(4)) :: a,b,c
        real_fox = a%data_a + b%data_a + c%data_a
        print *,"real_fox = ",real_fox
      end

      integer function real_cat(b)
        class(point(4)) :: b
        real_cat = b%data_a
        print *,"real_cat = ",real_cat
      end
      end module

      program t
        use m

        integer rc
        type(point(4)) :: tp1,tp2,tp3
        tp1 = point(4)(3)
        tp2 = point(4)(6)
        tp3 = point(4)(9)

        rc = tp1%foo(tp2)
        rc = tp2%bar(tp3)
        rc = tp3%fox(tp1,tp2)
      end program t
