      module mod
      interface
        function func1()
          procedure(len), pointer :: func1
        end function
      end interface
      end module

      use mod
      procedure(integer), pointer :: pp1
      character(10) :: char1
      integer kkk

      pp1 => func1()
      kkk = pp1(char1)
      if (kkk .ne. 10) error stop 11
      end

      function func1()
        procedure(len), pointer :: func1
        func1 => len
      end function
