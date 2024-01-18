!*  ===================================================================
!*
!*  DATE                       : 30/05/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Use Association, VOLATILE
!*  DESCRIPTION                :
!*           declare and use variable as VOLATILE in thread environment
!* ===================================================================

  module global
    use f_pthread
#ifdef bg4thd
    integer, parameter :: numthread=3
#else
    integer, parameter :: numthread=10
#endif
    integer   :: thread_id(numthread)
  end module

  program volatileMisc01

    use global
    implicit none

    integer p, i, rc, result
    type(f_pthread_t) thr(numthread)
    type(f_pthread_attr_t) attr

    common /cbk/ result

    external update

    VOLATILE result

    p = FLAG_DEFAULT
    do i = 1, numthread
      thread_id(i) = i
    enddo

    rc = f_pthread_attr_init(attr)
      if (rc <> 0) then
        print*, rc
        error stop 1_4
      endif

    do i = 1, numthread-1
      rc = f_pthread_create(thr(i), attr, flag=p, ent=update, arg=thread_id(i))
        if (rc <> 0) then
          print*, "can not create thread"
          error stop 2_4
        endif
    enddo

    !call update(thread_id(numthread))
    call update()

!    print *, result

    do i = 1, numthread-1
       rc = f_pthread_join(thr(i))
          if (rc <> 0) then
            print*, i, rc
            error stop 3_4
          endif
    enddo

  end program volatileMisc01

  !subroutine update (int)
  subroutine update ()
    use global
    integer result
    common /cbk/ result
    print *,  result

!    do while(.true.)
        result = result+1
!        call sleep(2)
!    end do

  end subroutine

  block data bk
    common /cbk/ result
    data result /0/
  end block data
