# Internal environment holding all registered hydrocan adapters keyed by name.
# Adapters are added via register_hydrocan_adapter() and read by the router.
.hydrocan_registry <- new.env(parent = emptyenv())
