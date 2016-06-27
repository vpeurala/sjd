import java.util.List;
import java.util.ArrayList;
import java.util.Optional;
import java.math.BigDecimal;
import java.util.Objects;

public class SubSpecialsBuilder {
    private String uid;
    private long itemId;
    private List<String> validSubProducts = new ArrayList<>();
    private long price;
    private boolean showView;
    private String description;
    private Optional<String> change = Optional.empty();
    private List<Customer> buyers = new ArrayList<>();
    private boolean booleanField;
    private byte byteField;
    private char charField;
    private short shortField;
    private int intField;
    private long longField;
    private float floatField;
    private double doubleField;
    private String stringField;
    private List<String> listField = new ArrayList<>();
    private Optional<String> optionalField = Optional.empty();
    private Object objectField;
    private BigDecimal value;

    public static SubSpecialsBuilder create() {
        return new SubSpecialsBuilder();
    }

    public static SubSpecialsBuilder from(SubSpecials subSpecials) {
        return new SubSpecialsBuilder().
            setUid(subSpecials.getUid()).
            setItemId(subSpecials.getItemId()).
            setValidSubProducts(subSpecials.getValidSubProducts()).
            setPrice(subSpecials.getPrice()).
            setShowView(subSpecials.isShowView()).
            setDescription(subSpecials.getDescription()).
            setChange(subSpecials.getChange()).
            setBuyers(subSpecials.getBuyers()).
            setBooleanField(subSpecials.isBooleanField()).
            setByteField(subSpecials.getByteField()).
            setCharField(subSpecials.getCharField()).
            setShortField(subSpecials.getShortField()).
            setIntField(subSpecials.getIntField()).
            setLongField(subSpecials.getLongField()).
            setFloatField(subSpecials.getFloatField()).
            setDoubleField(subSpecials.getDoubleField()).
            setStringField(subSpecials.getStringField()).
            setListField(subSpecials.getListField()).
            setOptionalField(subSpecials.getOptionalField()).
            setObjectField(subSpecials.getObjectField()).
            setValue(subSpecials.getValue());
    }

    public SubSpecialsBuilder setUid(String uid) {
        Objects.requireNonNull(uid);
        this.uid = uid;
        return this;
    }

    public SubSpecialsBuilder setItemId(long itemId) {
        Objects.requireNonNull(itemId);
        this.itemId = itemId;
        return this;
    }

    public SubSpecialsBuilder addToValidSubProducts(String member) {
        Objects.requireNonNull(validSubProducts);
        this.validSubProducts.add(member);
        return this;
    }

    public SubSpecialsBuilder setValidSubProducts(List<String> validSubProducts) {
        Objects.requireNonNull(validSubProducts);
        this.validSubProducts = validSubProducts;
        return this;
    }

    public SubSpecialsBuilder setPrice(long price) {
        Objects.requireNonNull(price);
        this.price = price;
        return this;
    }

    public SubSpecialsBuilder setShowView(boolean showView) {
        Objects.requireNonNull(showView);
        this.showView = showView;
        return this;
    }

    public SubSpecialsBuilder setDescription(String description) {
        Objects.requireNonNull(description);
        this.description = description;
        return this;
    }

    public SubSpecialsBuilder setChange(String change) {
        Objects.requireNonNull(change);
        this.change = Optional.of(change);
        return this;
    }

    public SubSpecialsBuilder setChange(Optional<String> change) {
        Objects.requireNonNull(change);
        this.change = change;
        return this;
    }

    public SubSpecialsBuilder withoutChange() {
        this.change = Optional.empty();
        return this;
    }

    public SubSpecialsBuilder addToBuyers(Customer member) {
        Objects.requireNonNull(buyers);
        this.buyers.add(member);
        return this;
    }

    public SubSpecialsBuilder setBuyers(List<Customer> buyers) {
        Objects.requireNonNull(buyers);
        this.buyers = buyers;
        return this;
    }

    public SubSpecialsBuilder setBooleanField(boolean booleanField) {
        Objects.requireNonNull(booleanField);
        this.booleanField = booleanField;
        return this;
    }

    public SubSpecialsBuilder setByteField(byte byteField) {
        Objects.requireNonNull(byteField);
        this.byteField = byteField;
        return this;
    }

    public SubSpecialsBuilder setCharField(char charField) {
        Objects.requireNonNull(charField);
        this.charField = charField;
        return this;
    }

    public SubSpecialsBuilder setShortField(short shortField) {
        Objects.requireNonNull(shortField);
        this.shortField = shortField;
        return this;
    }

    public SubSpecialsBuilder setIntField(int intField) {
        Objects.requireNonNull(intField);
        this.intField = intField;
        return this;
    }

    public SubSpecialsBuilder setLongField(long longField) {
        Objects.requireNonNull(longField);
        this.longField = longField;
        return this;
    }

    public SubSpecialsBuilder setFloatField(float floatField) {
        Objects.requireNonNull(floatField);
        this.floatField = floatField;
        return this;
    }

    public SubSpecialsBuilder setDoubleField(double doubleField) {
        Objects.requireNonNull(doubleField);
        this.doubleField = doubleField;
        return this;
    }

    public SubSpecialsBuilder setStringField(String stringField) {
        Objects.requireNonNull(stringField);
        this.stringField = stringField;
        return this;
    }

    public SubSpecialsBuilder addToListField(String member) {
        Objects.requireNonNull(listField);
        this.listField.add(member);
        return this;
    }

    public SubSpecialsBuilder setListField(List<String> listField) {
        Objects.requireNonNull(listField);
        this.listField = listField;
        return this;
    }

    public SubSpecialsBuilder setOptionalField(String optionalField) {
        Objects.requireNonNull(optionalField);
        this.optionalField = Optional.of(optionalField);
        return this;
    }

    public SubSpecialsBuilder setOptionalField(Optional<String> optionalField) {
        Objects.requireNonNull(optionalField);
        this.optionalField = optionalField;
        return this;
    }

    public SubSpecialsBuilder withoutOptionalField() {
        this.optionalField = Optional.empty();
        return this;
    }

    public SubSpecialsBuilder setObjectField(Object objectField) {
        Objects.requireNonNull(objectField);
        this.objectField = objectField;
        return this;
    }

    public SubSpecialsBuilder setValue(BigDecimal value) {
        Objects.requireNonNull(value);
        this.value = value;
        return this;
    }

    public SubSpecials build() {
        return new SubSpecials(
            this.uid,
            this.itemId,
            this.validSubProducts,
            this.price,
            this.showView,
            this.description,
            this.change,
            this.buyers,
            this.booleanField,
            this.byteField,
            this.charField,
            this.shortField,
            this.intField,
            this.longField,
            this.floatField,
            this.doubleField,
            this.stringField,
            this.listField,
            this.optionalField,
            this.objectField,
            this.value
        );
    }
}