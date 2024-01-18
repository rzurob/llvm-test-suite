!*********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qstrict -qarch=com
! %GROUP: fxpd0082.f
! %VERIFY: fxpd0082.out:fxpd0082.vf
! %STDIN:
! %STDOUT: fxpd0082.out
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*  AIX XL FORTRAN/6000 TEST CASE                 IBM INTERNAL USE ONLY
!*  ===================================================================
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Allan Zhang        
!*  DATE                       : October 01, 1996
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : PARALLEL DO
!*  SECONDARY FUNCTIONS TESTED : REDUCTION clause,do-loops,
!*                               allocatable arrays
!*  TEST CASE NAME             : independent/reduction/fxpd0082
!*
!*  DESCRIPTION                : Test PARALLEL DO with REDUCTION clause
!*                               where the REDUCTION named-variable is 
!*                               an array.  Test various
!*                               uses of that array within the DO-loop.
!*
!*                               Test REDUCTION statement through
!*                               various means:
!*                               elemental assignment, array section
!*                               assignment, whole array assignment,
!*                               assignment through a function.
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  10/01/96   AZ     -Initial Version
!*  05/22/97   KL     -Modified for PARALLEL DO directive
!*  07/28/98   VE     -Modified for PARALLEL DO DEFAULT and
!*		       FIRSTPRIVATE clauses 
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012
!
@process free(f90) intlog
program abstracti060
   implicit none
   integer i,j,k,l,m,n
   integer,parameter :: Nx=100 
   real u,v,w
   integer r,s,t
   integer :: ia(Nx)
   integer :: ia_new(Nx)
   real*16 :: r16a(Nx,Nx)
   real*16 :: r16a_new(Nx,Nx)
   complex :: ca(Nx/10,Nx/10,Nx/10)
   complex :: ca_new(Nx/10,Nx/10,Nx/10)
   logical :: la(Nx)
   logical :: la_new(Nx)

!234567--------------------------------------------------------------012
! Interface blocks.
!234567--------------------------------------------------------------012
   abstract interface
      recursive pure function func1ai( i ) result( func1_res )
         logical func1_res
         integer, intent(in) :: i
      end function func1ai
   end interface

   procedure (func1ai) :: func1

!234567--------------------------------------------------------------012
! Assign to the REDUCTION array via elemental assignment.
!234567--------------------------------------------------------------012

   ia = 0
   forall(i=1:Nx) ia_new(i) = i

!smp$ parallel do default(none), private( i ), reduction(+:ia),&
!smp$ shared(ia_new ) 
   do i = 1, 100
      ia(i) = ia(i) + (ia_new(i) * ia_new(i) - ia_new(i))
   enddo

   print *, 'ia --------------------------------'
   do i = 1, 100, 7
      print *, i, ia(i)
   enddo


!234567--------------------------------------------------------------012
! Assign to the REDUCTION array via array section assignment.
!234567--------------------------------------------------------------012

   r16a = 1.0q0

   do r = 1.0, 100.0, 2.0
      do s = 1.0, 100.0, 2.0
         r16a_new(r:r+1,s:s+1) = r+s
      enddo
   enddo

!smp$ parallel do default(shared), private( r,s),shared(r16a_new),&
!smp$ reduction(*:r16a)
   do r = 1.0, 100.0, 2.0
!smp$ parallel do private(s), firstprivate(r), default(none),& 
!smp$ reduction(*:r16a),shared(r16a_new)
      do s = 1.0, 100.0, 2.0
         r16a(r:r+1,s:s+1) = r16a(r:r+1,s:s+1) * s * r * s * r * s * r * s &
&                          / r16a_new(r:r+1,s:s+1) / r16a_new(r:r+1,s:s+1) &
&                          / r16a_new(r:r+1,s:s+1) / r16a_new(r:r+1,s:s+1) &
&                          / r16a_new(r:r+1,s:s+1) / r16a_new(r:r+1,s:s+1) &
&                          / r16a_new(r:r+1,s:s+1) / r16a_new(r:r+1,s:s+1) &
&                          / r16a_new(r:r+1,s:s+1) / r16a_new(r:r+1,s:s+1) &
&                          / r16a_new(r:r+1,s:s+1) / r16a_new(r:r+1,s:s+1) &
&                          / r16a_new(r:r+1,s:s+1) / r16a_new(r:r+1,s:s+1) &
&                          / r16a_new(r:r+1,s:s+1) / r16a_new(r:r+1,s:s+1) &
&                          / r16a_new(r:r+1,s:s+1) / r16a_new(r:r+1,s:s+1) &
&                          / r16a_new(r:r+1,s:s+1) / r16a_new(r:r+1,s:s+1)
      enddo
   enddo

   print *, 'r16a ------------------------------'
   do i = 1, 100, 7
      do j = 1, 100, 7
         print '(2i3,1x,e10.5e2)', i, j, r16a(i,j)
      enddo
   enddo


!234567--------------------------------------------------------------012
! Assign to the REDUCTION array via whole array assignment.
!234567--------------------------------------------------------------012

   ca = (0.0d0,0.0d0)
   forall( r=1.0:10.0,s=1.0:10.0,t=1.0:10.0) ca_new(r,s,t) = r*s*t

!smp$ parallel do default(none), private( r,s,t ), reduction(+:ca),&
!smp$ shared(ca_new)
   do r = 1.0, 10.0
!smp$ parallel do default(none), private( s,t ), reduction(+:ca)&
!smp$ firstprivate(r),shared(ca_new)
      do s = 1.0, 10.0
!smp$ parallel do default(none), private(t), reduction(+:ca),&
!smp$ firstprivate(r,s),shared(ca_new)
         do t = 1.0, 10.0
            ca(r,s,t) = ca(r,s,t) + ca_new(r,s,t)*ca_new(r,s,t)*ca_new(r,s,t)  &
&                     *ca_new(r,s,t)*ca_new(r,s,t)*ca_new(r,s,t)*ca_new(r,s,t) &
&                     *ca_new(r,s,t)*ca_new(r,s,t)*ca_new(r,s,t)               &
&                     /(ca_new(r,s,t)+ca_new(r,s,t)+ca_new(r,s,t)+ca_new(r,s,t)&
&                      +ca_new(r,s,t)+ca_new(r,s,t)+ca_new(r,s,t)+ca_new(r,s,t)&
&                      +ca_new(r,s,t)+ca_new(r,s,t)+ca_new(r,s,t)+ca_new(r,s,t)&
&                     )/real(r*s*t+r*s*t+r*s*t+r*s*t+r*s*t+r*s*t+r*s*t+r*s*t+r*s*t &
                        +r*s*t+r*s*t+r*s*t+r*s*t+r*s*t+r*s*t+r*s*t+r*s*t+r*s*t &
                        +r*s*t+r*s*t+r*s*t+r*s*t+r*s*t+r*s*t+r*s*t+r*s*t+r*s*t)
         enddo
      enddo
   enddo

   print *, 'ca --------------------------------'
   do i = 1, 10, 3
      do j = 1, 10, 3
         do k = 1, 10, 3
            print '(3i3,2x,e10.5e2)', i, j, k, ca(i,j,k)
         enddo
      enddo
   enddo


!234567--------------------------------------------------------------012
! Assign to the REDUCTION array via a function.
!234567--------------------------------------------------------------012

   la = .FALSE.
   forall(i=1:Nx) la_new(i) = func1( mod(i,2) )

!smp$ parallel do default(shared), private(i), reduction(+:la),&
!smp$ shared(la_new)
   do i = 1, 100
      la(i) = la(i) + la_new(i)
   enddo

   print *, 'la --------------------------------'
   do i = 1, 100, 7
      print *, i, la(i)
   enddo


end program abstracti060

@process free(f90), intlog
recursive pure function func1( i ) result( func1_res )

   logical func1_res
   integer, intent(in) :: i

   if (i .eq. 0) then
      func1_res = 1
   else
      func1_res =  - i*8**0 + i*8**1 - i*8**2 + i*8**3 - i*4**4 + i*4**5 &
&                   - i*4**6 + i*4**7 - i*2**8 + i*2**9 - i*2**10 + i*2**11 &
&                   - i*1**12 + i*1**13 - i*1**14 + i*1**15 + func1(i-1)
   end if

end function func1
