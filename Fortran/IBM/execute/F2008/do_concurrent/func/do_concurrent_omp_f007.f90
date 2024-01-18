!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : August 24, 2015
!*
!*  PRIMARY FUNCTIONS TESTED   : DO CONCURRENT
!*  SECONDARY FUNCTIONS TESTED : OMP SINGLE
!*
!*  DESCRIPTION                : Test DO CONCURRENT construct in SINGLE
!*                               construct in module function
!*
!* ===================================================================

module fxsg004bmod
  implicit none
  integer   i,j,k
  complex   carr1(50), carr2(50)

  contains

   function mod_func1(carray1)
     complex   carray1(50), mod_func1(50)
     complex   tmpc1, tmpc2, tmparray1(5), tmparray2(50)
     real   tmpr1, tmpr2
     integer i, j, k, l

     !$omp parallel
     l = 10
     !$omp single private(i), firstprivate(l)
     k = 0
     do concurrent (i = l:l+10)
        k = k + i
        do concurrent (j = 1:10)
           k = k + j
        end do
     end do
     !$omp end single

     ! initialize different sections of the array
     !$omp single private(i)
     do concurrent (i = 1:25)
        carray1(i) = (0.0e0, 1.0e1)
     end do
     !$omp end single nowait

     !$omp single private(i)
     do concurrent (i = 26:50)
        carray1(i) = (real(i), real(-1*i))
     end do
     !$omp end single

     !$omp single private(i, tmpc1)
     tmpc1 = (0.0e0, 0.0e0)
     do concurrent (i = 1:10)
        tmpc1 = sum(carray1)
     end do
     tmparray1(1) = tmpc1
     !$omp end single nowait

     !$omp single private(i,j, tmpc1)
     tmpc1 = (1.0e0, -1.0e0)
     do concurrent (i = 1:100)
        do concurrent (j = 1:50)
           tmpc1 = tmpc1 + (real(i), real(j))
        end do
     end do
     tmparray1(2) = tmpc1
     !$omp end single nowait

     !$omp single private(i,tmpc1)
     tmpr1 = -1000.0e0
     tmpr2 = -1.0e0
     do concurrent (i = 2:50:2)
        tmpr1 = min (tmpr1, (-1)*imag(carray1(i)))
     end do
     do concurrent (i = 1:50:2)
        tmpr1 = min (tmpr1, (-1)*imag(carray1(i)))
     end do
     tmpc1 = (tmpr1, tmpr2)
     tmparray1(3) = tmpc1
     !$omp end single nowait

     !$omp single private(i,tmpc1)
     tmpc1 = (1.0e0, 1.0e0)
     do concurrent (i = 1:50:10)
        tmpc1 = tmpc1 * carray1(i)
     end do
     tmparray1(4) = tmpc1
     !$omp end single nowait

     !$omp single
     tmparray1(5) = (1.5e0, -2.0e1)
     !$omp end single

     !$omp single
     print*, tmparray1
     !$omp end single

     !$omp single
     tmparray2 = (/(tmparray1, i=1, 10, 1)/)
     print*, tmparray2
     mod_func1 = carray1 + tmparray2
     !$omp end single
     !$omp end parallel

   end function

   function mod_func2(carray1)
     complex   carray1(50), mod_func2(50)

     mod_func2 = carray1
   end function mod_func2

end module fxsg004bmod

program fxsg004b

   use fxsg004bmod

   carr2 =  mod_func1(carr1) ! call to module function
   print*, carr2

   carr1 =  mod_func2(carr2) ! call to module function
   print*, carr1

end program fxsg004b

