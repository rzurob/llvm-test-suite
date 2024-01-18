!* =================================================================== &
!*
!* DATE                       : March 10, 2011
!* ORIGIN                     : AIX Compiler Development,
!*
!* PRIMARY FUNCTIONS TESTED   : Intrinsic types in TYPE spec
!*
!* DESCRIPTION                : Testing proper functionality of
!*                              Intrinsic types in TYPE spec
!*                              with implicit
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      program intrinsictype18f

      class(*), allocatable :: p
      implicit type(integer) (i)
      implicit type(real) (r)
      implicit type(character(3)) (c)
      implicit type(complex) (m)
      implicit type(logical) (l)
      implicit type(byte) (b)

      i = 1
      r = 2.0
      c = "abc"
      m = (3.0,4.0)
      l = .true.
      b = 5

      allocate(p,source=i)

      select type(p)
      type is(integer)
        print *,"correct"
      class default
        stop 1
      end select

      deallocate (p)
      allocate(p,source=r)

      select type(p)
      type is(real)
        print *,"correct"
      class default
        stop 2
      end select

      deallocate (p)
      allocate(p,source=c)

      select type(p)
      type is(character(*))
        print *,"correct"
      class default
        stop 3
      end select

      deallocate (p)
      allocate(p,source=m)

      select type(p)
      type is(complex)
        print *,"correct"
      class default
        stop 4
      end select

      deallocate (p)
      allocate(p,source=l)

      select type(p)
      type is(logical)
        print *,"correct"
      class default
        stop 5
      end select

      deallocate (p)
      allocate(p,source=b)

      select type(p)
      type is(byte)
        print *,"correct"
      class default
        stop 6
      end select

      end
