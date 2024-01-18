! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/misc/d245836.f
! opt variations: -qnol

!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 04/03/2007
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : defect 245836 (or 190336)
!                               reduced test case from original TC given in
!                               defect 245836.  Defect 190336 was opened against
!                               aix.os: _xlqdiv is not threadsafe.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

    
   program fxpr003e
   implicit none
   complex(4) diff
   logical(4) precision_x8

   interface
      function ext_func(base1, base2, base3, N)
         integer, intent(in):: base1, base2, base3, N
         complex ext_func
      end function
   end interface
 
    diff = ext_func(2, 4, 8, 100)

    if (.not. precision_x8(diff, cmplx(1,100))) error stop 1_4
   end

   function ext_func(base1, base2, base3, N)
      implicit none
      integer, intent(in) :: base1, base2, base3, N
      type t1(n1,k1,k2,k3)    ! (20,4,8,16)
         integer, kind :: k1,k2,k3
         integer, len  :: n1
         real(k1)         r4
         real(k2)         r8
         real(k3)         r16
         double precision dp
      end type t1
      type (t1(20,4,8,16)) t_array(10*N)
      complex(4) ext_func
      integer size, i
      real*16 tmp3, sarray3(10*N), reali16
      logical precision_r8

      size   = 10*N

!$omp parallel private(reali16, tmp3)
!$omp do
      do i = 1, size
         reali16        = i
         t_array(i)%r16 = qlog(reali16) - qlog(8.0q0)
      enddo
!$OMP end do

!$omp DO
      do i = 1, size
         tmp3       = t_array(i)%r16 + qlog(8.0q0)
         sarray3(i) = base3 ** (tmp3 / qlog(8.0q0))
      enddo
!$OMP END DO

!$omp end parallel

      do i = 1, size
        if (.not. precision_r8(real(sarray3(i),8), real(i,8))) then
           print*, sarray3(i), i
           error stop 7_4
        endif

     enddo

     ext_func = cmplx(sarray3(1), sarray3(N))
   end function
