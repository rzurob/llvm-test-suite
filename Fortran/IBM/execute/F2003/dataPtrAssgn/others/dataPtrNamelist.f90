!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrNamelist.f
!*
!*  DATE                       : Aug 31, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION
!*
!* - data-ptr is a namelist group object
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main

    type base
        character(4), allocatable :: tar(:)
    end type

    character(:), pointer :: ptr(:)

    namelist /nlst/  ptr

    type(base), target :: b

    allocate( b%tar(4), source =  (/ '1234', 'ABCD', '5678','OPQR' /) )

    ptr(2:5) => b%tar

    if ( .not. associated(ptr, b%tar) ) stop 2
    if ( lbound(ptr,1) /= 2 ) stop 5
    if ( ubound(ptr,1) /= 5 ) stop 8

    open (5, file = 'dataPtrNamelist.out', form ='formatted', access='stream' )

    write(5, nml=nlst)


 End program

