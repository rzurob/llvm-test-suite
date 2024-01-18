!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrPackOther.f
!*
!*  DATE                       : Aug 31, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION
!*
!* - data-targets are assumed-shape arrays, dummy args of external proc
!* - data-pointers are module vars, associated with targets in external proc
!* - type logical, real, & complex
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
    logical, pointer :: lp(:,:,:)
    real, pointer :: rp(:)
    complex, pointer :: cp(:,:)
end module

    program main
	use m
        interface foo
            subroutine sub1(a)
                logical, target :: A(2:,1:,3: )
            end subroutine

            subroutine sub2(lb, a)
		integer lb
		real, target :: A(lb:)
            end subroutine

            subroutine sub3(a)
		complex, target :: A(:,:)
	    end subroutine
        end interface

        logical, target :: lt(1:5,11:13,21:22)
        real, target :: rt(101:120)
	complex, target :: ct(-3:-1, 0:4)

        lt = reshape( (/( mod(i,2)==0 ,i=1,30)/), (/5,3,2/))
	rt = (/ ( real(i), i=31,50) /)
	ct =  reshape( (/(cmplx(i,i+1,4)  ,i=1,15)/), (/3,5/))

        call sub1(lt)

	call foo(11,rt)

	call foo(ct)

	! verify association status & bound info of lp, rp, cp
        if ( .not. associated(lp, lt) ) stop 9
        print *, lbound(lp)
        print *, ubound(lp)

        if ( .not. associated(rp) ) stop 19
        print *, lbound(rp,1)
        print *, ubound(rp,1)

        if ( .not. associated(cp) ) stop 29
        print *, lbound(cp)
        print *, ubound(cp)

	! verify the value of lp, rp, cp
	print *, lp
	print *, pack(lp,lp)
        write(*, '(5f10.3)') rp
        write(*, '(6f10.3)') pack(rp, (/ ( i*7 > i*i , i=1,10 ) /) )
	write (*, '("(",f10.3,", ", f10.3, ")")')  cp
	write (*, '("(",f10.3,", ", f10.3, ")")')  pack(cp, .true.)

    end program

    subroutine sub1(a)
        use m, only : lp

        logical, target :: A(2:,1:,3: )

        lp(lbound(a,1):, lbound(a,2):, lbound(a,3):) => a

    end subroutine

    subroutine sub2(lb, a)
	use m, only : rp
	integer lb
	real, target :: A(lb:)

	rp(lb:ubound(a,1)-10) => a(::1)
    end subroutine

    subroutine sub3(a)
	use m, only : cp
	complex, target :: A(:,:)

	cp(ubound(a,1):ubound(a,1), lbound(a,2):ubound(a,2)) &
		 => a(2, ubound(a,2):lbound(a,2):-1)

    end subroutine
