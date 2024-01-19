!**********************************************************************
!*  ===================================================================
!*
!*  DATE                      : August 25, 2015
!*
!*  PRIMARY FUNCTIONS TESTED  : THREADPRIVATE, COPYIN
!*  SECONDARY FUNCTIONS TESTED: SECTIONS, PARALLEL SECTIONS, PARALLEL DO
!*				CRITICAL, ATOMIC
!*
!*  REFERENCE                 : threadprivate/exec/fxthp0207.f
!*
!*  DESCRIPTION               : Various parallel regions  constructs
!*                              with THREADPRIVATE common blocks.
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
!
      program fxthp0207
        implicit none
	integer(kind=8),parameter ::n=10
        complex(kind=8)           ::f1(n),f2(n),f3(n)
        complex(kind=8)           ::x(n),y(n),z(n)
        complex(kind=8)           ::w1,w2,w3
        complex(kind=8)           ::im
        real(kind=8)	          ::pi,dummy
	logical                   ::precision_r8
	common /frequency/ w1,w2,w3,f1,f2,f3
	common /coord/ x,y,z
	common /constants/ im,pi
!$omp threadprivate (/frequency/,/coord/,/constants/)

        im=(0.d0,1.d0)

	call init_values

!$omp parallel default(private),&
!$omp copyin(/frequency/,/constants/)
!$omp master
          f1=im*(x*w1+pi)
          f2=im*(y*w2+pi**2)
          f3=im*(z*w3+pi**3)
!$omp end master
!$omp end parallel

	call fields

!$omp parallel private(dummy),copyin(f1,f2,f3)
!$omp sections private(dummy)
!$omp section
	  dummy=outer_cmplx_sum(f1,f2,f3)
            if(.not.precision_r8(dummy,318.309886183787d0)) error stop 1
!$omp section
	  dummy=outer_cmplx_sum(f1,f2,f3)
            if(.not.precision_r8(dummy,318.309886183787d0)) error stop 2
!$omp section
	  dummy=outer_cmplx_sum(f1,f2,f3)
            if(.not.precision_r8(dummy,318.309886183787d0)) error stop 3
!$omp section
	  dummy=outer_cmplx_sum(f1,f2,f3)
            if(.not.precision_r8(dummy,318.309886183787d0)) error stop 4
!$omp section
	  dummy=outer_cmplx_sum(f1,f2,f3)
            if(.not.precision_r8(dummy,318.309886183787d0)) error stop 5
!$omp end sections
!$omp end parallel

	contains
          real(kind=8) function outer_cmplx_sum(f1,f2,f3)
            complex(kind=8)           ::f1(:),f2(:),f3(:)
	    real(kind=8)		  ::cmplx_sum

!$omp critical
	    outer_cmplx_sum=cmplx_sum(f1,f2,f3)
!$omp end critical

            end  function outer_cmplx_sum
      end program fxthp0207

      subroutine init_values
        implicit none
	integer(kind=8),parameter ::n=10
	complex(kind=8)           ::E0(n,n,n)
	complex(kind=8)           ::E1(n,1,1),E2(1,n,1),E3(1,1,n)
        complex(kind=8)           ::im
        complex(kind=8)           ::f1(n),f2(n),f3(n)
        complex(kind=8)           ::x(n),y(n),z(n)
        complex(kind=8)           ::w1,w2,w3
        real(kind=8)	          ::pi,cmplx_sum,dummy
	integer(kind=4)           ::i,j,k
	logical                   ::precision_r8
	common /frequency/ w1,w2,w3,f1,f2,f3
	common /field/ E1,E2,E3
	common /coord/ x,y,z
	common /constants/ im,pi
!$omp threadprivate (/frequency/,/field/,/coord/,/constants/)
	common /totfield/ E0

	pi=4.d0*datan(1.d0)

!$omp parallel copyin(pi)
	  w1=2.d0/(pi**2+pi**3+pi**4)
          w2=pi**3
	  w3=1.d0/dsqrt(pi)
!$omp end parallel

!$omp parallel default(private),copyin(pi),shared(E0)
!$omp single
	  do concurrent (i = 1:n, j = 1:n, k = 1:n)
            E0(i,j,k) = 1.d0/dsqrt(pi)
          end do
!$omp end single
	  do concurrent (i = 1:n)
            x(i) = -pi+float(i)*2.d0*pi/n
          end do
	  do concurrent (j = 1:n)
            y(j) = float(j)*2.d0*pi/n
          end do
	  do concurrent (k = 1:n)
            z(k) = pi-float(k)*2.d0*pi/n
          end do
!$omp end parallel

       end subroutine init_values

      subroutine fields
        implicit none
	integer(kind=8),parameter ::n=10
	complex(kind=8)           ::E1(n,1,1),E2(1,n,1),E3(1,1,n)
        complex(kind=8)           ::im
        complex(kind=8)           ::f1(n),f2(n),f3(n)
        complex(kind=8)           ::x(n),y(n),z(n)
        complex(kind=8)           ::w1,w2,w3
        real(kind=8)	          ::pi
	integer(kind=4)           ::i,j,k
	common /frequency/ w1,w2,w3,f1,f2,f3
	common /field/ E1,E2,E3
	common /coord/ x,y,z
	common /constants/ im,pi
!$omp threadprivate (/frequency/,/coord/,/constants/,/field/)

!$omp parallel copyin(im,/coord/,/frequency/)
          do concurrent (i = 1:n)
	    E1(i,1,1)=cdexp(f1(i)+im*w1*x(i))
          end do
          do concurrent (j = 1:n)
	    E2(1,j,1)=cdexp(f2(j)+im*w2*y(j))
          end do
          do concurrent (k = 1:n)
	    E3(1,1,k)=cdexp(f3(k)+im*w3*z(k))
          end do
!$omp end parallel

      end subroutine fields

      real(kind=8) function cmplx_sum(f1,f2,f3)
        implicit none
	integer(kind=8),parameter ::n=10
	complex(kind=8)           ::E0(n,n,n)
	complex(kind=8)           ::E1(n,1,1),E2(1,n,1),E3(1,1,n)
        complex(kind=8)           ::f1(n),f2(n),f3(n)
	integer(kind=4)           ::i,j,k
	common /field/ E1,E2,E3
	common /totfield/ E0
!$omp threadprivate(/field/)

!$omp parallel sections copyin(/field/) shared(E0)
!$omp section
	  do concurrent (i = 1:n)
            do concurrent (j = 1:n)
              do concurrent (k = 1:n)
!$atomic
                E0(i,j,k)=E0(i,j,k)*E1(k,1,1)
              end do
            end do
          end do
!$omp end parallel sections

	  do concurrent (i = 1:n)
            do concurrent (j = 1:n)
              do concurrent (k = 1:n)
!$omp parallel sections default(private),copyin(E1,E2,E3),shared(E0),&
!$omp firstprivate(i,j,k)
!$omp section
!$atomic
                E0(i,j,k)=E0(i,j,k)*E1(k,1,1)
!$omp end parallel sections
              end do
            end do
          end do

!$omp parallel sections default(shared) copyin(E1,E2,E3)
!$omp section
!$omp parallel copyin(E1,E2,E3)
	  do concurrent (i = 1:n)
            do concurrent (j = 1:n)
              do concurrent (k = 1:n)
!$atomic
                E0(i,j,k)=E0(i,j,k)*E1(k,1,1)
              end do
            end do
          end do
!$omp end parallel
!$omp end parallel sections

	cmplx_sum = sum((real(E0)**2+imag(E0)**2))

      return
      end function cmplx_sum

