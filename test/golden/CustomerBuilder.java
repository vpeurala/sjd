import java.util.Objects;

public class CustomerBuilder {
    private String firstName;
    private String lastName;
    private String uid;

    public static CustomerBuilder create() {
        return new CustomerBuilder();
    }

    public static CustomerBuilder from(Customer customer) {
        return new CustomerBuilder().
            setFirstName(customer.getFirstName()).
            setLastName(customer.getLastName()).
            setUid(customer.getUid());
    }

    public CustomerBuilder setFirstName(String firstName) {
        Objects.requireNonNull(firstName);
        this.firstName = firstName;
        return this;
    }

    public CustomerBuilder setLastName(String lastName) {
        Objects.requireNonNull(lastName);
        this.lastName = lastName;
        return this;
    }

    public CustomerBuilder setUid(String uid) {
        Objects.requireNonNull(uid);
        this.uid = uid;
        return this;
    }

    public Customer build() {
        return new Customer(
            this.firstName,
            this.lastName,
            this.uid
        );
    }
}