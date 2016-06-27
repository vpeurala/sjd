import java.util.List;
import java.util.ArrayList;
import java.util.Optional;
import java.util.Objects;

public class SpecialsBuilder {
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

    public static SpecialsBuilder create() {
        return new SpecialsBuilder();
    }

    public static SpecialsBuilder from(Specials specials) {
        return new SpecialsBuilder().
            setUid(specials.getUid()).
            setItemId(specials.getItemId()).
            setValidSubProducts(specials.getValidSubProducts()).
            setPrice(specials.getPrice()).
            setShowView(specials.isShowView()).
            setDescription(specials.getDescription()).
            setChange(specials.getChange()).
            setBuyers(specials.getBuyers()).
            setBooleanField(specials.isBooleanField()).
            setByteField(specials.getByteField()).
            setCharField(specials.getCharField()).
            setShortField(specials.getShortField()).
            setIntField(specials.getIntField()).
            setLongField(specials.getLongField()).
            setFloatField(specials.getFloatField()).
            setDoubleField(specials.getDoubleField()).
            setStringField(specials.getStringField()).
            setListField(specials.getListField()).
            setOptionalField(specials.getOptionalField()).
            setObjectField(specials.getObjectField());
    }

    public SpecialsBuilder setUid(String uid) {
        Objects.requireNonNull(uid);
        this.uid = uid;
        return this;
    }

    public SpecialsBuilder setItemId(long itemId) {
        Objects.requireNonNull(itemId);
        this.itemId = itemId;
        return this;
    }

    public SpecialsBuilder addToValidSubProducts(String member) {
        Objects.requireNonNull(validSubProducts);
        this.validSubProducts.add(member);
        return this;
    }

    public SpecialsBuilder setValidSubProducts(List<String> validSubProducts) {
        Objects.requireNonNull(validSubProducts);
        this.validSubProducts = validSubProducts;
        return this;
    }

    public SpecialsBuilder setPrice(long price) {
        Objects.requireNonNull(price);
        this.price = price;
        return this;
    }

    public SpecialsBuilder setShowView(boolean showView) {
        Objects.requireNonNull(showView);
        this.showView = showView;
        return this;
    }

    public SpecialsBuilder setDescription(String description) {
        Objects.requireNonNull(description);
        this.description = description;
        return this;
    }

    public SpecialsBuilder setChange(String change) {
        Objects.requireNonNull(change);
        this.change = Optional.of(change);
        return this;
    }

    public SpecialsBuilder setChange(Optional<String> change) {
        Objects.requireNonNull(change);
        this.change = change;
        return this;
    }

    public SpecialsBuilder withoutChange() {
        this.change = Optional.empty();
        return this;
    }

    public SpecialsBuilder addToBuyers(Customer member) {
        Objects.requireNonNull(buyers);
        this.buyers.add(member);
        return this;
    }

    public SpecialsBuilder setBuyers(List<Customer> buyers) {
        Objects.requireNonNull(buyers);
        this.buyers = buyers;
        return this;
    }

    public SpecialsBuilder setBooleanField(boolean booleanField) {
        Objects.requireNonNull(booleanField);
        this.booleanField = booleanField;
        return this;
    }

    public SpecialsBuilder setByteField(byte byteField) {
        Objects.requireNonNull(byteField);
        this.byteField = byteField;
        return this;
    }

    public SpecialsBuilder setCharField(char charField) {
        Objects.requireNonNull(charField);
        this.charField = charField;
        return this;
    }

    public SpecialsBuilder setShortField(short shortField) {
        Objects.requireNonNull(shortField);
        this.shortField = shortField;
        return this;
    }

    public SpecialsBuilder setIntField(int intField) {
        Objects.requireNonNull(intField);
        this.intField = intField;
        return this;
    }

    public SpecialsBuilder setLongField(long longField) {
        Objects.requireNonNull(longField);
        this.longField = longField;
        return this;
    }

    public SpecialsBuilder setFloatField(float floatField) {
        Objects.requireNonNull(floatField);
        this.floatField = floatField;
        return this;
    }

    public SpecialsBuilder setDoubleField(double doubleField) {
        Objects.requireNonNull(doubleField);
        this.doubleField = doubleField;
        return this;
    }

    public SpecialsBuilder setStringField(String stringField) {
        Objects.requireNonNull(stringField);
        this.stringField = stringField;
        return this;
    }

    public SpecialsBuilder addToListField(String member) {
        Objects.requireNonNull(listField);
        this.listField.add(member);
        return this;
    }

    public SpecialsBuilder setListField(List<String> listField) {
        Objects.requireNonNull(listField);
        this.listField = listField;
        return this;
    }

    public SpecialsBuilder setOptionalField(String optionalField) {
        Objects.requireNonNull(optionalField);
        this.optionalField = Optional.of(optionalField);
        return this;
    }

    public SpecialsBuilder setOptionalField(Optional<String> optionalField) {
        Objects.requireNonNull(optionalField);
        this.optionalField = optionalField;
        return this;
    }

    public SpecialsBuilder withoutOptionalField() {
        this.optionalField = Optional.empty();
        return this;
    }

    public SpecialsBuilder setObjectField(Object objectField) {
        Objects.requireNonNull(objectField);
        this.objectField = objectField;
        return this;
    }

    public Specials build() {
        return new Specials(
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
            this.objectField
        );
    }
}