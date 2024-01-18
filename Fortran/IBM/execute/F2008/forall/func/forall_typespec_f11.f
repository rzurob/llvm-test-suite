!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : forall_typespec_f11.f
!*
!*  PROGRAMMER                 : Bernard Kan
!*  DATE                       : 2012-06-25
!*  ORIGIN                     : 
!*
!*  PRIMARY FUNCTIONS TESTED   : FORALL with type specifier (F2008 extension)
!*  SECONDARY FUNCTIONS TESTED : Nesting in omp parallel workshare, where
!*  ADAPTED FROM               : wkshare/test/fxws12h.scenario
!*
!*  DESCRIPTION
!*
!*    OMP Parallel Workshare with where and forall
!*
!* =============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890

      program fxws12h
      implicit none

      integer, parameter::l=10, m=20, n=16
      integer :: i=1, j=2, k=3, shiftA(l,n)
      real arrayrelA(l,m,n), arrayrelB(l,m,n)
      complex arraycpxA(l,m,n),arraycpxB(l,m,n)
      integer arrayintA(l,m,n), arrayintB(l,m,n)
      logical arraylogA(l,m,n), arraylogB(l,m,n)
      character arraychaA(l,m,n), arraychaB(l,m,n)
      
      real EPSILON /5.E-1/

      call random_number(arrayrelA)
      call random_number(arrayrelB)

!$omp parallel workshare

      forall(integer::i=1:l,j=1:m,k=1:n, arrayrelA(i,j,k) .gt.
     + real(0.01))
           where(arrayrelA(i,j,:) .gt. EPSILON) 
             arrayrelA(i,j,:) = 12*maxval(arrayrelA)+arrayrelB(i,j,k)
           elsewhere(arrayrelA(i,j,:) .eq. EPSILON)
             arrayrelA(i,j,:) = real(2.0)
           elsewhere 
             arrayrelA(i,j,:) = 140*minval(arrayrelA)+arrayrelB(i,j,k)
           end where       
      end forall

!$omp end parallel workshare

      write(*, '(20f9.3)') arrayrelA(:,2:3,12)

!$omp parallel
!$omp workshare

      arrayintA = int(arrayrelA + sum(arrayrelA)/1000)
      arrayintB = int(arrayrelA + sum(arrayrelA)/1000) 

      forall(integer(2)::i=1:l,j=1:m,k=1:n, arrayintA(i,j,k) .gt.
     + 20)
           where(arrayintB(i,j,:) .lt. 30)
             arrayintA(i,j,:) = arrayintB(i,j,k)
           elsewhere(arrayintB(i,j,:) .eq. 30)
             arrayintA(i,j,:) = int(2)
           elsewhere
             arrayintA(i,j,:) = minval(arrayintA)+arrayintB(i,j,k)
           end where
      end forall

!$omp end workshare
!$omp end parallel 

      write(*, '(20i4)') arrayintA(:,2:3,12)

!$omp parallel workshare

      arraycpxA = cmplx(2, 3) 

      forall(integer(4)::i=1:l,j=1:m)
           where(arraycpxA(i,j,:) .eq. cmplx(2, 3))
             arraycpxB(i,j,:) = arraycpxA(i,j,:) + conjg(cmplx(2, 3))
           elsewhere
             arraycpxA(i,j,:) = cmplx(1,1)
           end where
      end forall

!$omp end parallel workshare

      print *, arraycpxA(:,2:3,12)

!$omp parallel
!$omp workshare

      arraylogA = .true.
      shiftA        = 0
      shiftA(2:5,:) = 5
      shiftA(:,6:9) = 9
      shiftA(10, :) = -4
!     arraylogB = eoshift(arraylogA, shift=(/5,9,-4/), dim=2)
      arraylogB = eoshift(arraylogA, shiftA, dim=2)

      forall(integer(2)::i=1:l,j=1:m,k=1:n, arraylogA(i,j,k) 
     + .eqv. .true.)
           where(arraylogB(i,j,:) .eqv. .false.)
             arraylogB(i,j,:) = .true.
           end where
      end forall

!$omp end workshare
!$omp end parallel

       print *, count(arraylogB .eqv. .true.)

!$omp parallel workshare

      arraychaA = reshape((/(((achar(i+73),i=1,l),j=1,m),k=1,n)/),      &
     + (/l,m,n/))

      forall(integer*1::i=1:l,j=1:m)
           where(arraychaA(i,j,:) .lt. achar(78))
             arraychaB(i,j,:) = arraychaA(i,j,:)
           elsewhere(arraychaA(i,j,:) .eq. achar(78))
             arraychaB(i,j,:) = achar(74)
           elsewhere
             arraychaB(i,j,:) = achar(73)
           end where
      end forall

!$omp end parallel workshare

      write(*, '(20a2)') arraychaB(:,2:3,12)

       if (i.ne.1) then
           print*,"Forall index modified the external variable i=",i
           error stop 66
       endif
       if (j.ne.2) then
          print*,"Forall index modified the external variable j=",j
          error stop 77
       endif
       if (k.ne.3) then
           print*,"Forall index modified the external variable k=",k
           error stop 88
       endif
       end program fxws12h


