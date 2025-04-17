<div class="ginger-edit__aside-item ginger-edit__aside--publicationdate" id="ginger-edit__aside--publicationdate">

    <h3>{_ Publication date _}</h3>
    <div>
        <input
            type="date"
            name="dt:ymd:0:publication_start"
            value="{{ id.publication_start|date:'Y-m-d' }}"
            class="form-control"
        >
    </div>
</div>