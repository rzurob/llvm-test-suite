      !!! Implement Fortran overloading-interfaces in C
      !!! call from Fortran
      !
      ! This trick works only in one direction:
      ! Fortran calling multiple C functions using one name.
      !
      ! The reverse (C calling multiple Fortran functions using one name)
      ! could be done via the C preprocessor but we won't bother because
      ! it wouldn't be testing anything new on the xlf side.

      program p1
        interface f1  ! interface with overloading
          function f1a()   bind(c, name='f1a_B')
          end function

          function f1b(%val(a))  bind(c, name='f1b_B')
          end function

          function f1c(%val(a),%val(b))  bind(c, name='f1c_B')
          end function
        end interface


        !! Call directly, by-passing the Fortran overloading interface
        print*, 'rv =', f1a()
        print*, 'rv =', f1b(2.0)
        print*, 'rv =', f1c(2.0, 3.0)

        !! Go through the Fortran overloading interface
        print*, 'rv =', f1()
        print*, 'rv =', f1(2.0)
        print*, 'rv =', f1(2.0, 3.0)
      end program
