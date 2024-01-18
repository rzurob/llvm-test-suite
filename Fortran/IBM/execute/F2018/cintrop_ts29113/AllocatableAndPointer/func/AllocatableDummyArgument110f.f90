! *********************************************************************
!* ===================================================================
!* XL Fortran Test Case                         IBM INTERNAL USE ONLY
!* ===================================================================
!*
!* TEST CASE TITLE              : AllocatableDummyArgument110f.f
!*
!* PROGRAMMER                   : Dorra Bouchiha
!* DATE                         : January 25, 2013
!* ORIGIN                       : AIX Complier Development
!*                              : IBM Software Solutions Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED     : C Interop: ALLOCATABLE and POINTER dummy argument
!* SECONDARY FUNTIONS TESTED    :
!*
!* DRIVER STANZA                :
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : Calling a Fortran BIND(C) procedure from Fortran
!*
!*                                - Allocatable Scalar/Array of various interoperable types
!*                                - List-directed read 
!*                                - Formatted write 
!*                                - move_alloc
!*
!234567890123456789012345678901234567890123456789012345678901234567890
subroutine my_print(a, b, unit) bind(c)
     use iso_c_binding
     implicit none
     integer(c_int), allocatable, intent(in) :: a
     real(c_double), allocatable, intent(in) :: b(:)
     integer, intent(in) :: unit

     if (.not. allocated(a)) then
         write (unit, '(a)', advance='no') 'id is not allocated; '
     else
         write (unit, '(a, i10)', advance='no') 'id = ', a
     end if

     if (allocated(b)) then
         write (unit, '(a,2(1x, i7), 1x, a)', advance='no') '; bounds of data:',&
                 lbound(b,1), ubound(b,1), ', values are :'

         write (unit, '(1x, 5g20.12)') b
     else
         write (unit, *) 'data is not allocated'
     end if
end subroutine

subroutine my_read(a, b, unit) bind(c)
     use iso_c_binding
     implicit none
     integer(c_int), allocatable, intent(out) :: a
     real(c_double), allocatable, intent(out) :: b(:)
     integer, intent(in) :: unit
     integer :: iostat
     character(256) :: iomsg
     integer lb, ub

     if (.not. allocated(a)) then 
        allocate(a)
     else 
        error stop 10
     end if 
     read (unit, *, iostat=iostat, iomsg=iomsg) a , lb, ub
     if (iostat /= 0) return

     call readDataArray (lb, ub)

     contains

     subroutine readDataArray(lb, ub)
         integer, intent(in) :: lb, ub
         real(8), allocatable :: localData(:)

         allocate(localData(lb:ub))
         read (unit, *, iostat=iostat, iomsg=iomsg) localData
         if (iostat /= 0) return
         if (allocated(b)) then 
             error stop 11
         else
            call move_alloc(localData, b)
         end if
         if (  .not. allocated(b)) error stop 12
         if (allocated(localData)) error stop 13
     end subroutine
end subroutine my_read

program AllocatableDummyArgument110f
use iso_fortran_env
implicit none

    integer i, j
    integer, allocatable :: id
    real(8), allocatable :: data(:)

    interface
       subroutine my_print(a, b, unit) bind(c)
            use iso_c_binding
            implicit none
            integer(c_int), allocatable, intent(in) :: a
            real(c_double), allocatable, intent(in) :: b(:)
            integer, intent(in) :: unit
       end subroutine my_print
       subroutine my_read(a, b, unit) bind(c)
            use iso_c_binding
            implicit none
            integer(c_int), allocatable, intent(out) :: a
            real(c_double), allocatable, intent(out) :: b(:)
            integer, intent(in) :: unit
       end subroutine my_read
    end interface

    do i =1, 8
        write (1,*) i*100, 0, i 
        write (1,*) (j*1.1d0, j=1,i+1)
    end do
    
    rewind(1)
    
    do i = 1, 8
        call my_read(id, data, 1)
        call my_print(id, data, OUTPUT_UNIT)
    end do

end program AllocatableDummyArgument110f
