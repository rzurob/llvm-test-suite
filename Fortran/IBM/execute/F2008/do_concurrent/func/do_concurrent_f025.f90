!*******************************************************************************
!*
!============================================================================
!USE ONLY
!*
!============================================================================
!*
!*  TEST CASE NAME             : do_concurrent_f025.f
!*
!*  DATE                       : 2015-03-24
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   : DO CONCURRENT (F2008 extension)
!*  SECONDARY FUNCTIONS TESTED :
!*  ADAPTED FROM               :
!*
!*  DESCRIPTION                :
!*    - acceptance of do concurrent loop with a large number of indices with OMP
!*    - acceptance of deep nesting with OMP of
!*      - do concurrent
!*      - do concurrent in other loop structures
!*
!=============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890

      program main
        implicit none

        integer :: a = 100, b = 100, c = 100, d = 100, e = 100, f = 100, g = 100, h = 100, i = 100, j = 100, k = 100, l = 100, m = 100, n = 100, o = 100, p = 100, q = 100, r = 100, s = 100, t = 100, u = 100, v = 100, w = 100, x = 100, y = 100, z = 100
        logical :: res(2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2)

!****************************************************************************!
!*    - acceptance of do concurrent loop with a large number of indices      !
!****************************************************************************!
        do concurrent (a = 1:2, b = 1:2, c = 1:2, d = 1:2, e = 1:2, f = 1:2, g = 1:2, h = 1:2, i = 1:2, j = 1:2, k = 1:2, l = 1:2, m = 1:2, n = 1:2, o = 1:2, p = 1:2, q = 1:2, r = 1:2, s = 1:2, t = 1:2)
          res(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t)=.true.
        end do

        if (any(res .eqv. .false.)) error stop 11

!****************************************************************************!
!*    - large number of nested loops                                         !
!****************************************************************************!
        do concurrent (a = 1:2, b = 1:2)
          do concurrent (c = 1:2, d = 1:2)
            do concurrent (e = 1:2, f = 1:2)
              do concurrent(g = 1:2, h = 1:2)
                do concurrent(i = 1:2, j = 1:2)
                  do concurrent (k = 1:2, l = 1:2)
                    do concurrent (m = 1:2, n = 1:2)
                      do concurrent (o = 1:2, p = 1:2)
                        do concurrent(q = 1:2, r = 1:2)
                          do concurrent(s = 1:2, t = 1:2)
                            res(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t)=.false.
                          end do
                        end do
                      end do
                    end do
                  end do
                end do
              end do
            end do
          end do
        end do

        if (any(res .eqv. .true.)) error stop 22

!****************************************************************************!
!*    - large number of mixed nested loops                                   !
!****************************************************************************!
        do concurrent (a = 1:2, b = 1:2)
!!          !$OMP PARALLEL SHARED(res)
          do c = 1,2
            do concurrent (d = 1:2)
              do concurrent (e = 1:2, f = 1:2)
!!                !$OMP PARALLEL PRIVATE(g) SHARED(res)
                do g = 1,2
                  do concurrent(h = 1:2)
                    do concurrent(i = 1:2, j = 1:2)
!!                      !$OMP PARALLEL PRIVATE(k) SHARED(res)
                      do k = 1,2
                        do concurrent (l = 1:2)
                          do concurrent (m = 1:2, n = 1:2)
                            !$OMP PARALLEL PRIVATE(o) SHARED(res)
                            do o = 1,2
                              do concurrent (p = 1:2)
                                !$OMP PARALLEL PRIVATE(q) SHARED(res)
                                do q = 1,2
                                  do concurrent(r = 1:2, s = 1:2)
                                    forall(t = 1:2)
                                      res(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t)=.true.
                                    end forall
                                  end do
                                end do
                                !$OMP END PARALLEL
                              end do
                            end do
                            !$OMP END PARALLEL
                          end do
                        end do
                      end do
!!                      !$OMP END PARALLEL
                    end do
                  end do
                end do
!!                !$OMP END PARALLEL
              end do
            end do
          end do
!!          !$OMP END PARALLEL
        end do

        if (any(res .eqv. .false.)) error stop 33
      end
