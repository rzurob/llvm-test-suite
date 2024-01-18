!*********************************************************************
!**********************************************************************
!*  ===================================================================
!*
!*  PRIMARY FUNCTIONS TESTED  : Automatic deallocation of allocatable
!*                              array. External recursive function.
!*
!*  DESCRIPTION               : On each call to the procedure memory is
!*                              allocated for the ALLOCATBLE array. If
!*                              an arry is not deallocated on exit from
!*              the procedure, the program will crash
!*                              during run time.
!*  STRUCTURE                  : Main program
!*  EXECUTABLE                 : Yes
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  03/11/01   1.0    -Modified Version
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
!
       program allocfun001
        implicit none
        real(kind=8)                 ::dummy,func_ext
        integer,parameter            ::Nx=10,Ny=10,Nz=3,Nw=18
        complex(kind=16),allocatable ::c(:,:,:,:)
    integer(kind=4)              ::i,stat

        type dt_m
        integer, allocatable :: b
        logical, allocatable :: c
        end type dt_m
        type(dt_m), allocatable :: dt_1
!........................................................................
!   The recursive function func_ext() makes two iterations (number
!       of succesfull iterations is defined  by the -bmaxdata: value)
!       allocating storage for a new copy of the array a(:,:,:,:)
!       on each iteration.
!........................................................................

        dummy=func_ext()
!........................................................................
!   Allocation of the array c(:,:,:,:) wiil fail, if the array a
!   wil not be deallocated on exit from the function func_ext().
!........................................................................

        allocate(c(Nx/2,0:Ny,(1+Nz)*4,Nw),stat=i)
        if(i.ne.0) error stop 10

        allocate(dt_1)
        if (.not. allocated(dt_1)) error stop 11

       end program allocfun001

       recursive real(kind=8) function func_ext()
        implicit none
        integer           ::i,stat
        integer,save      ::counter
        data counter/0/
        integer,parameter        ::Nx=10,Ny=10,Nz=3,Nw=18
        real(kind=8),allocatable ::a(:,:,:,:)
        real(kind=8),allocatable ::d(:,:,:,:)
        real(kind=8)::e(1:10,1:10,1:10,1:10)

        type dt_type
        integer, allocatable :: b
        logical, allocatable :: c
        end type dt_type
        type(dt_type), allocatable :: dt

        counter=counter+1

!       allocate(a(Nx/2,0:Ny,(1+Nz)*4,Nw),d(Nx/2,0:Ny,(1+Nz)*4,Nw),source=e,stat=i)
        allocate(a(1:10,1:10,1:10,1:10),d(1:10,1:10,1:10,1:10),source=e,stat=i)
        deallocate(a); deallocate(d)
        allocate(a(1:10,1:10,1:10,1:10),d(1:10,1:10,1:10,1:10),mold=e,stat=i)

        if(i.ne.0) call zzrc(counter)

        allocate(dt)
        if(.not. allocated(dt)) call zzrc(counter)

        if(counter.lt.3) then
       func_ext=func_ext()
        endif
        return
       end function func_ext
