import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

public class Customer {
    private final String firstName;
    private final String lastName;
    private final String uid;

    @JsonCreator
    public Customer(@JsonProperty("firstName") String firstName,
                    @JsonProperty("lastName") String lastName,
                    @JsonProperty("uid") String uid) {
        Objects.requireNonNull(firstName, "Property 'firstName' cannot be null.");
        Objects.requireNonNull(lastName, "Property 'lastName' cannot be null.");
        Objects.requireNonNull(uid, "Property 'uid' cannot be null.");
        this.firstName = firstName;
        this.lastName = lastName;
        this.uid = uid;
    }

    public String getFirstName() {
        return firstName;
    }

    public String getLastName() {
        return lastName;
    }

    public String getUid() {
        return uid;
    }

    @Override
    public String toString() {
        return "Customer@" + System.identityHashCode(this) + ": {"
            + "firstName = '" + getFirstName() + "'"
            + "lastName = '" + getLastName() + "'"
            + "uid = '" + getUid() + "'"
        ;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null) return false;
        if (this.getClass() != o.getClass()) return false;
        Customer that = (Customer) o;
        if (!this.getFirstName().equals(that.getFirstName())) return false;
        if (!this.getLastName().equals(that.getLastName())) return false;
        if (!this.getUid().equals(that.getUid())) return false;
        return true;
    }

    @Override
    public int hashCode() {
        int result = 0;
        result = 31 * result + getFirstName().hashCode();
        result = 31 * result + getLastName().hashCode();
        result = 31 * result + getUid().hashCode();
        return result;
    }
}