program aoc11_part2
  use iso_fortran_env
  implicit none

  integer :: stat
!  integer, parameter :: max_i=10, max_j=10
  integer, parameter :: max_i=140, max_j=140
  character(1) :: input(max_i, max_j)
  integer :: distmapx(max_i, max_j), distmapy(max_i, max_j)
  character(1), allocatable :: expanded(:,:)
  integer :: new_i, new_j, nexp_i, nexp_j
  integer, allocatable :: expand_pos_i(:), expand_pos_j(:)
  integer, allocatable :: galpos(:,:)
  integer :: n_gal, expansion = 1000000

!  open(10, file="input_small.txt", status="old", iostat=stat)
  open(10, file="input.txt", status="old", iostat=stat)

  block
    integer :: i, j
    character(max_j) :: line

    n_gal = 0
    do j=1,max_j
        read(10, '(A)') line
        do i=1,max_i
          input(i,j) = line(i:i)
          if (input(i,j) == "#") n_gal = n_gal + 1
        end do
    end do
    close(10)
    
    distmapx = 1
    distmapy = 1

    allocate(expand_pos_i(0), expand_pos_j(0))
    do j = 1, max_j
      if (all(input(:,j) == '.')) then
        expand_pos_j = [j, expand_pos_j]
        distmapy(:,j) = expansion
      end if

    end do
    do i = 1, max_i
      if (all(input(i,:) == '.')) then
        expand_pos_i = [i, expand_pos_i]
        distmapx(i,:) = expansion
      end if
    end do
    print *, "expand_pos_j", expand_pos_j
    print *, "expand_pos_i", expand_pos_i

    nexp_i = size(expand_pos_i, dim=1)
    new_i = max_i + nexp_i
    nexp_j = size(expand_pos_j, dim=1)
    new_j = max_j + nexp_j
    print *, "new_i", new_i
    print *, "new_j", new_j
    print *, "n_gal", n_gal

    allocate(expanded(new_i, new_j))
    expanded = '.'
  end block

  block
    integer :: ii, jj
    integer :: i, j
    integer :: ni, nj
    integer :: n_pos(2, n_gal), n_pos2(2, n_gal)
    integer :: i_gal
    integer :: distances(n_gal, n_gal)
    integer(int64) :: distances2(n_gal, n_gal)

    nj=1
    i_gal=0
    do j = new_j, 1, -1
      jj = j - nexp_j + nj - 1
      if (expand_pos_j(nj) == jj) then 
        nj = nj + 1
        cycle
      end if

      ni=1
      do i = new_i, 1, -1
        ii = i - nexp_i + ni - 1
        if (expand_pos_i(ni) == ii) then
          ni = ni + 1
          cycle
        end if
        expanded(i, j) = input(ii, jj)
        if (input(ii,jj) == "#") then
          i_gal = i_gal + 1
          n_pos(1, i_gal) = i
          n_pos(2, i_gal) = j
          n_pos2(1, i_gal) = ii
          n_pos2(2, i_gal) = jj
        end if
      end do
    end do

!    write(*, "(A5,X,*(I3))") "n_pos(1)", n_pos(1,:)
!    write(*, "(A5,X,*(I3))") "n_pos(2)", n_pos(2,:)


    distances = 0
    distances2 = 0
    do i = 1, n_gal
      do j = i+1, n_gal
        distances(i, j) = sum(abs(n_pos(:,i)-n_pos(:,j))) ! manhattan distance
        block
          integer li,ui, lj,uj
          li = min(n_pos2(1,i),n_pos2(1,j))
          ui = max(n_pos2(1,i),n_pos2(1,j))
          lj = min(n_pos2(2,i),n_pos2(2,j))
          uj = max(n_pos2(2,i),n_pos2(2,j))
          distances2(i, j) = sum(distmapx(li:ui-1,lj))
          distances2(i, j) = distances2(i, j) + sum(distmapy(ui, lj:uj-1))
          
!          print *, li, ui
        end block
      end do
    end do

    print *, "sum of distances:", sum(distances, distances>0)
    print *, "sum of distances2:", sum(distances2, distances2>0)

!  call print_map(input)
!  call print_map_int(distmapx)
!  call print_map_int(distmapy)
!  call print_map(expanded)
!  call print_map_int(distances2)

  end block




contains

    subroutine print_map(map)
      character(1), intent(in) :: map(:,:)
      integer :: i, j

      do j = 1, size(map, dim=2)
        print *, map(:,j)
      end do
    end subroutine

    subroutine print_map_int(map)
      integer, intent(in) :: map(:,:)
      integer :: i, j

      do j = 1, size(map, dim=2)
        write(*, "(*(I3))") map(:,j)
      end do
    end subroutine

end program aoc11_part2
