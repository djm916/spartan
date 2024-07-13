package spartan.errors;

import java.io.IOException;
import java.io.FileNotFoundException;
import java.nio.file.NoSuchFileException;
import java.nio.file.FileAlreadyExistsException;
import java.nio.file.InvalidPathException;
import java.nio.file.AccessDeniedException;
import java.nio.channels.ClosedChannelException;

public class IOError extends Error
{
  public enum ErrorCode {
    FILE_NOT_FOUND("file not found"),
    FILE_EXISTS("file exists"),
    PORT_CLOSED("port closed"),
    ACCESS_DENIED("access denied"),
    INVALID_PATH("invalid path"),
    UNSUPPORTED("unsupported operation"),
    OTHER("");
    
    ErrorCode(String message) {
      this.message = message;
    }
    
    private String message;
  }
  
  private static ErrorCode initErrorCode(Throwable ex)
  {
    return switch (ex) {
      case NoSuchFileException e -> ErrorCode.FILE_NOT_FOUND;
      case FileNotFoundException e -> ErrorCode.FILE_NOT_FOUND;
      case FileAlreadyExistsException e -> ErrorCode.FILE_EXISTS;
      case ClosedChannelException e -> ErrorCode.PORT_CLOSED;
      case AccessDeniedException e -> ErrorCode.ACCESS_DENIED;
      case InvalidPathException e -> ErrorCode.INVALID_PATH;
      default -> ErrorCode.OTHER;
    };
  }
    
  public int errorNo()
  {
    return errorCode.ordinal();
  }
  
  public IOError(Throwable cause)
  {
    this(initErrorCode(cause));
  }
  
  public IOError(ErrorCode code)
  {
    super(code.message);
    this.errorCode = code;
  }
  
  private final ErrorCode errorCode;
}
