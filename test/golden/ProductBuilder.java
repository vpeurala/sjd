import java.util.List;
import java.util.ArrayList;
import java.util.Optional;
import java.util.Objects;

public class ProductBuilder {
    private String uid;
    private long itemId;
    private List<String> validSubProducts = new ArrayList<>();
    private long price;
    private boolean showView;
    private String description;
    private Optional<String> change = Optional.empty();
    private List<Customer> buyers = new ArrayList<>();

    public static ProductBuilder create() {
        return new ProductBuilder();
    }

    public static ProductBuilder from(Product product) {
        return new ProductBuilder().
            setUid(product.getUid()).
            setItemId(product.getItemId()).
            setValidSubProducts(product.getValidSubProducts()).
            setPrice(product.getPrice()).
            setShowView(product.isShowView()).
            setDescription(product.getDescription()).
            setChange(product.getChange()).
            setBuyers(product.getBuyers());
    }

    public ProductBuilder setUid(String uid) {
        Objects.requireNonNull(uid);
        this.uid = uid;
        return this;
    }

    public ProductBuilder setItemId(long itemId) {
        Objects.requireNonNull(itemId);
        this.itemId = itemId;
        return this;
    }

    public ProductBuilder addToValidSubProducts(String member) {
        Objects.requireNonNull(validSubProducts);
        this.validSubProducts.add(member);
        return this;
    }

    public ProductBuilder setValidSubProducts(List<String> validSubProducts) {
        Objects.requireNonNull(validSubProducts);
        this.validSubProducts = validSubProducts;
        return this;
    }

    public ProductBuilder setPrice(long price) {
        Objects.requireNonNull(price);
        this.price = price;
        return this;
    }

    public ProductBuilder setShowView(boolean showView) {
        Objects.requireNonNull(showView);
        this.showView = showView;
        return this;
    }

    public ProductBuilder setDescription(String description) {
        Objects.requireNonNull(description);
        this.description = description;
        return this;
    }

    public ProductBuilder setChange(String change) {
        Objects.requireNonNull(change);
        this.change = Optional.of(change);
        return this;
    }

    public ProductBuilder setChange(Optional<String> change) {
        Objects.requireNonNull(change);
        this.change = change;
        return this;
    }

    public ProductBuilder withoutChange() {
        this.change = Optional.empty();
        return this;
    }

    public ProductBuilder addToBuyers(Customer member) {
        Objects.requireNonNull(buyers);
        this.buyers.add(member);
        return this;
    }

    public ProductBuilder setBuyers(List<Customer> buyers) {
        Objects.requireNonNull(buyers);
        this.buyers = buyers;
        return this;
    }

    public Product build() {
        return new Product(
            this.uid,
            this.itemId,
            this.validSubProducts,
            this.price,
            this.showView,
            this.description,
            this.change,
            this.buyers
        );
    }
}