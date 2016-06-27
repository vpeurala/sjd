import java.util.List;
import java.util.Optional;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

public class Product extends Item {
    private final List<String> validSubProducts;
    private final long price;
    private final boolean showView;
    private final String description;
    private final Optional<String> change;
    private final List<Customer> buyers;

    @JsonCreator
    public Product(@JsonProperty("uid") String uid,
                   @JsonProperty("itemId") long itemId,
                   @JsonProperty("validSubProducts") List<String> validSubProducts,
                   @JsonProperty("price") long price,
                   @JsonProperty("showView") boolean showView,
                   @JsonProperty("description") String description,
                   @JsonProperty("change") Optional<String> change,
                   @JsonProperty("buyers") List<Customer> buyers) {
        super(uid, itemId);
        Objects.requireNonNull(validSubProducts, "Property 'validSubProducts' cannot be null.");
        Objects.requireNonNull(price, "Property 'price' cannot be null.");
        Objects.requireNonNull(showView, "Property 'showView' cannot be null.");
        Objects.requireNonNull(description, "Property 'description' cannot be null.");
        Objects.requireNonNull(change, "Property 'change' cannot be null.");
        Objects.requireNonNull(buyers, "Property 'buyers' cannot be null.");
        this.validSubProducts = validSubProducts;
        this.price = price;
        this.showView = showView;
        this.description = description;
        this.change = change;
        this.buyers = buyers;
    }

    public List<String> getValidSubProducts() {
        return validSubProducts;
    }

    public long getPrice() {
        return price;
    }

    public boolean isShowView() {
        return showView;
    }

    public String getDescription() {
        return description;
    }

    public Optional<String> getChange() {
        return change;
    }

    public List<Customer> getBuyers() {
        return buyers;
    }

    @Override
    public String toString() {
        return "Product@" + System.identityHashCode(this) + ": {"
            + "uid = '" + getUid() + "'"
            + "itemId = '" + getItemId() + "'"
            + "validSubProducts = '" + getValidSubProducts() + "'"
            + "price = '" + getPrice() + "'"
            + "showView = '" + isShowView() + "'"
            + "description = '" + getDescription() + "'"
            + "change = '" + getChange() + "'"
            + "buyers = '" + getBuyers() + "'"
        ;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null) return false;
        if (this.getClass() != o.getClass()) return false;
        Product that = (Product) o;
        if (!this.getUid().equals(that.getUid())) return false;
        if (this.getItemId() != that.getItemId()) return false;
        if (!this.getValidSubProducts().equals(that.getValidSubProducts())) return false;
        if (this.getPrice() != that.getPrice()) return false;
        if (this.isShowView() != that.isShowView()) return false;
        if (!this.getDescription().equals(that.getDescription())) return false;
        if (!this.getChange().equals(that.getChange())) return false;
        if (!this.getBuyers().equals(that.getBuyers())) return false;
        return true;
    }

    @Override
    public int hashCode() {
        int result = 0;
        result = 31 * result + getUid().hashCode();
        result = 31 * Long.hashCode(this.getItemId());
        result = 31 * result + getValidSubProducts().hashCode();
        result = 31 * Long.hashCode(this.getPrice());
        result = 31 * result + (this.isShowView() ? 1 : 0);
        result = 31 * result + getDescription().hashCode();
        result = 31 * result + getChange().hashCode();
        result = 31 * result + getBuyers().hashCode();
        return result;
    }
}