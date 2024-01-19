
module verify_field_mod
    use constants_mod, only: double, pi
    contains

    subroutine verify_field (field, n, f, x0, x1, tol)
        integer, intent(in) :: n
        real(double), intent(in) :: field(n)[1,*]
        real(double), intent(in) :: x0, x1, tol

        interface
            real(double) function f (x)
            import double
                real(double), intent(in) :: x
            end function
        end interface

        real(double) dx, xi, scale_factor
        integer i, me, np, img

        me = this_image()
        np = num_images()

        scale_factor = scaling_func()

        if (me == np) then
            dx = (x1 - x0) /n/np
            xi = x0
            do img = 1, num_images()
                do i = 1, size(field)
                    xi = xi + dx

                    if (.not. my_precision_r8(field(i)[1,img], f(xi), &
                            scale_factor*tol)) then
                        print *, 'failed to verify element', i, 'on image', img
                        print *, field(i)[1,img], 'vs', f(xi)
                        error stop 1
                    end if

                end do
            end do
        end if

        contains

        real(double) function scaling_func ()
            scaling_func = 1.0d0 + sqrt(abs(np - 8)*1.0d0)*2.0d0
        end function
    end subroutine

    logical function my_precision_r8 (a, b, tol)
        real(8), intent(in) :: a, b, tol

        if (abs(a - b) < tol) then
            my_precision_r8 = .true.
            return
        end if

        my_precision_r8 = abs(a - b) <= abs(a + b) * 0.5d0 * tol
    end function
end
