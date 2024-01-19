      program t
      use, intrinsic :: iso_c_binding
      implicit none
        character(len=1), parameter :: ch1 = "ab"
        character(len=1) :: ch2
        character(c_char) :: ch3
        character(c_char), parameter :: ch4 = "abc"
        character(3) :: ch5
        character(1) :: ch6(10)

        integer(c_int), parameter :: cint1 = 10
        real(c_float), parameter :: creal1 = 10.0
        complex(c_float_complex), parameter :: com1 = (1,1)
        logical(c_bool), parameter ::logc1 =.true.

        integer(c_size_t) :: rt

        !character
        rt = c_sizeof("a")
        if (rt /= 1) then
          print *, rt
          error stop 1
        endif

        rt = c_sizeof(ch1)
        if (rt /= 1) then
          print *, rt
          error stop 2
        endif

        rt = c_sizeof(ch2)
        if (rt /= 1) then
          print *, rt
          error stop 3
        endif

        rt = c_sizeof(ch3)
        if (rt /= 1) then
          print *, rt
          error stop 4
        endif

        rt = c_sizeof(ch4)
        if (rt /= 1) then
          print *, rt
          error stop 5
        endif

        !integer
        rt = c_sizeof(cint1)
        if (rt /= 4) then
          print *, rt
          error stop 6
        endif

        rt = c_sizeof(10)
        if (rt /= 4) then
          print *, rt
          error stop 7
        endif

        !real
        rt = c_sizeof(creal1)
        if (rt /= 4) then
          print *, rt
          error stop 8
        endif

        rt = c_sizeof(10.0d0)
        if (rt /= 8) then
          print *, rt
          error stop 9
        endif

        rt = c_sizeof(10.0e0)
        if (rt /= 4) then
          print *, rt
          error stop 10
        endif

        !complex
        rt = c_sizeof(com1)
        if (rt /= 8) then
          print *, rt
          error stop 11
        endif

        rt = c_sizeof((12d0, 10.0d0))
        if (rt /= 16) then
          print *, rt
          error stop 12
        endif

        !logical
        !see detail in logical.f

        call sub0()
        call sub1(ch5)
        call sub2(ch6)
        call sub3(ch6)
        call sub4(ch6)
      end

      subroutine sub0() ! substring
      use,intrinsic :: iso_c_binding
      implicit none
        integer(c_size_t) rt
        character(len=10),parameter:: str1 ="123456789012345"
        character(len=1) :: str2
        character(len=20) :: str3 = "abc"
        character(len=1),parameter:: str4 ="a"

        rt = c_sizeof(str1(1:1))
        if (rt /= 1) then
          print *, rt
          error stop 13
        endif

        rt = c_sizeof(str2)
        if (rt /= 1) then
          print *, rt
          error stop 14
        endif

        rt = c_sizeof(str2(1:1))
        if (rt /= 1) then
          print *, rt
          error stop 15
        endif

        rt = c_sizeof(str3(1:1))
        if (rt /= 1) then
          print *, rt
          error stop 16
        endif

        rt = c_sizeof(str4)
        if (rt /= 1) then
          print *, rt
          error stop 17
        endif

        rt = c_sizeof(str4(1:1))
        if (rt /= 1) then
          print *, rt
          error stop 18
        endif
      end subroutine

      subroutine sub1(a)
      use, intrinsic :: iso_c_binding
      implicit none
      character(*) a
      integer i
      i = c_sizeof(a(1:1))
      if (i /= 1) then
        print *, i
        error stop 19
      endif
      end subroutine

      subroutine sub2(a)
        use, intrinsic :: iso_c_binding
        implicit none
        character(1) a(*)
        integer i, j
        j = 2
        i = c_sizeof(a(j))
        if (i /= 1) then
          print *, i
          error stop 20
        endif
      end subroutine

      subroutine sub3(a)
        use, intrinsic :: iso_c_binding
        implicit none
        character(1), allocatable :: a(:)
        integer i, j
        j = 2
        i = c_sizeof(a(j))
        if (i /= 1) then
          print *, i
          error stop 21
        endif
      end subroutine

      subroutine sub4(a)
        use, intrinsic :: iso_c_binding
        implicit none
        character(1) a(*)
        integer i
        i = c_sizeof(a(2))
        if (i /= 1) then
          print *, i
          error stop 22
        endif
      end subroutine
