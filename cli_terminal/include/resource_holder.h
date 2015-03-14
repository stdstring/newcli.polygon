#ifndef H_RESOURCE_HOLDER
#define H_RESOURCE_HOLDER

#include <functional>

namespace cli_terminal
{

template <typename R> class resource_holder
{
public:
    resource_holder() = delete;
    resource_holder(const resource_holder&) = delete;
    resource_holder(resource_holder&&) = delete;
    resource_holder& operator=(const resource_holder&) = delete;
    resource_holder& operator=(resource_holder&&) = delete;

    resource_holder(R resource, std::function<void(R)> resource_cleaner) : _resource(resource), _resource_cleaner(resource_cleaner) {}
    ~resource_holder() { _resource_cleaner(_resource); }

    // TODO (std_string) : think about change const to volatile
    R get() /*const*/ volatile { return _resource; }
private:
    R _resource;
    std::function<void(R)> _resource_cleaner;
};

}

#endif