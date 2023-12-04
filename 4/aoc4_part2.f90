program aoc4_part2
  implicit none

  character(116) :: line
  integer :: stat, answer
  integer :: n_cards(196)

  open(10, file='input.txt', status='old', iostat=stat)

  block
    character(5) :: game
    character(1) :: tmp
    character(3) :: tmp2

    integer :: igame, wnum(10), numbers(25)
    integer :: i, j, n_winning, k
    
    n_cards = 1

    k = 0
    do while (.true.)
      k=k+1
      read (10, '(A5,I3,A1,10I3,A3,25I3)', end=99) game, igame, tmp, wnum, tmp2, numbers

      n_winning = 0
      do i=1,10
        do j=1,25
          if (wnum(i) == numbers(j)) n_winning = n_winning + 1
        end do
      end do

      if (n_winning>0) then
        n_cards(k+1:k+n_winning) = n_cards(k+1:k+n_winning) + n_cards(k)
      end if
    end do


  end block
  99 continue

  print *, sum(n_cards)
end

