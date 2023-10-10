package spartan.util;

import java.util.Optional;

public interface Either<L, R> {
  default Optional<L> left() { return Optional.empty();}
  default Optional<R> right() { return Optional.empty();}

  static <L, R> Either<L, R> fromLeft(L left) {
    return new Either<L, R>() {
      @Override public Optional<L> left() { return Optional.of(left); }
    };
  }

  static <L, R> Either<L, R> fromRight(R right) {
    return new Either<L, R>() {
      @Override public Optional<R> right() { return Optional.of(right); }
    };
  }
}
